#!/usr/bin/env python3
"""Small httpbin-compatible fixture for Request-FP's integration tests."""

import argparse
import hashlib
import json
import sys
import threading
import time
from email import policy
from email.parser import BytesParser
from http.cookies import SimpleCookie
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlsplit


def single_values(values):
    return {key: items[-1] for key, items in values.items()}


FLAKY_COUNTS = {}
FLAKY_LOCK = threading.Lock()


class RequestHandler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, _format, *_args):
        return

    def send_bytes(self, status, body, content_type):
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        if body:
            self.wfile.write(body)

    def send_json(self, payload, status=200):
        body = json.dumps(
            payload, ensure_ascii=False, separators=(",", ":")
        ).encode("utf-8")
        self.send_bytes(status, body, "application/json; charset=utf-8")

    def request_details(self):
        parsed = urlsplit(self.path)
        return {
            "args": single_values(
                parse_qs(
                    parsed.query,
                    keep_blank_values=True,
                    encoding="utf-8",
                )
            ),
            "headers": dict(self.headers.items()),
            "url": "http://{}{}".format(
                self.headers.get("Host", "127.0.0.1"),
                self.path,
            ),
        }

    def read_body(self):
        length = int(self.headers.get("Content-Length", "0"))
        return self.rfile.read(length) if length else b""

    def parse_body(self, body):
        content_type = self.headers.get("Content-Type", "")
        form = {}
        files = {}
        file_types = {}
        json_body = None

        if content_type.startswith("application/json"):
            if body:
                try:
                    json_body = json.loads(body.decode("utf-8"))
                except json.JSONDecodeError:
                    json_body = None
        elif content_type.startswith("multipart/form-data"):
            message = BytesParser(policy=policy.default).parsebytes(
                (
                    "Content-Type: {}\r\n"
                    "MIME-Version: 1.0\r\n\r\n"
                ).format(content_type).encode("ascii")
                + body
            )
            for part in message.iter_parts():
                name = part.get_param(
                    "name", header="content-disposition"
                )
                if not name:
                    continue
                value = part.get_payload(decode=True).decode(
                    "utf-8", errors="replace"
                )
                if part.get_filename() is None:
                    form[name] = value
                else:
                    files[name] = value
                    file_types[name] = part.get_content_type()
        else:
            form = single_values(
                parse_qs(
                    body.decode("utf-8"),
                    keep_blank_values=True,
                    encoding="utf-8",
                )
            )

        return form, files, file_types, json_body

    def send_request_echo(self):
        details = self.request_details()
        details.update(
            {
                "data": "",
                "files": {},
                "form": {},
                "json": None,
            }
        )
        self.send_json(details)

    def send_body_echo(self):
        body = self.read_body()
        form, files, file_types, json_body = self.parse_body(body)
        details = self.request_details()
        details.update(
            {
                "data": body.decode("utf-8", errors="replace"),
                "files": files,
                "file_types": file_types,
                "form": form,
                "json": json_body,
            }
        )
        self.send_json(details)

    def send_redirect(self, location, cookies=()):
        self.send_response(302)
        self.send_header("Location", location)
        for cookie in cookies:
            self.send_header("Set-Cookie", cookie)
        self.send_header("Content-Length", "0")
        self.end_headers()

    def send_cookie_routes(self, path):
        """Cookie and redirect helpers. Returns True if handled."""
        args = single_values(
            parse_qs(urlsplit(self.path).query, keep_blank_values=True)
        )
        if path == "/set-cookie":
            # /set-cookie?name=value[&name2=value2]
            body = b'{"ok":true}'
            self.send_response(200)
            for key, value in args.items():
                self.send_header("Set-Cookie", "{}={}; Path=/".format(key, value))
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        elif path == "/expire-cookie":
            # /expire-cookie?name=sid&mode=max-age|expires
            name = args.get("name", "")
            if args.get("mode") == "expires":
                attr = "Expires=Thu, 01 Jan 1970 00:00:00 GMT"
            else:
                attr = "Max-Age=0"
            body = b'{"ok":true}'
            self.send_response(200)
            self.send_header("Set-Cookie", "{}=; {}; Path=/".format(name, attr))
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        elif path == "/redirect-set-cookie":
            # /redirect-set-cookie?name=n&value=v&to=/cookies
            self.send_redirect(
                args.get("to", "/cookies"),
                ["{}={}; Path=/".format(args.get("name", "c"),
                                        args.get("value", "1"))],
            )
        elif path == "/redirect-to":
            self.send_redirect(args.get("url", "/get"))
        else:
            return False
        return True

    def send_status(self, path):
        """Handle /status/<code> for any method. Returns True if handled."""
        if not path.startswith("/status/"):
            return False
        try:
            status = int(path.rsplit("/", 1)[1])
        except ValueError:
            status = 400
        # Drain any request body so the connection stays usable.
        self.read_body()
        if status in (204, 304):
            self.send_bytes(status, b"", "application/json; charset=utf-8")
        else:
            self.send_json({"status": status}, status)
        return True

    def do_GET(self):
        path = urlsplit(self.path).path

        if (self.send_status(path) or self.send_cookie_routes(path)
                or self.send_flaky(path)):
            pass
        elif path.startswith("/delay/"):
            try:
                seconds = min(float(path.rsplit("/", 1)[1]), 10.0)
            except ValueError:
                seconds = 1.0
            time.sleep(seconds)
            self.send_request_echo()
        elif path.startswith("/bytes/"):
            # Deterministic binary body: bytes 0..255 repeated.
            try:
                count = int(path.rsplit("/", 1)[1])
            except ValueError:
                count = 256
            body = bytes(i % 256 for i in range(count))
            self.send_bytes(200, body, "application/octet-stream")
        elif path.startswith("/stream-bytes/"):
            # Same bytes as /bytes/N, sent chunked without Content-Length.
            count = int(path.rsplit("/", 1)[1])
            body = bytes(i % 256 for i in range(count))
            self.send_response(200)
            self.send_header("Content-Type", "application/octet-stream")
            self.send_header("Transfer-Encoding", "chunked")
            self.end_headers()
            for start in range(0, count, 1000):
                chunk = body[start:start + 1000]
                self.wfile.write(b"%x\r\n%s\r\n" % (len(chunk), chunk))
            self.wfile.write(b"0\r\n\r\n")
        elif path.startswith("/truncated-bytes/"):
            # Promises N bytes, sends half, then closes the connection.
            count = int(path.rsplit("/", 1)[1])
            body = bytes(i % 256 for i in range(count // 2))
            self.send_response(200)
            self.send_header("Content-Type", "application/octet-stream")
            self.send_header("Content-Length", str(count))
            self.send_header("Connection", "close")
            self.end_headers()
            self.wfile.write(body)
            self.close_connection = True
        elif path == "/health":
            self.send_bytes(200, b"ok", "text/plain; charset=utf-8")
        elif path == "/html":
            self.send_bytes(
                200,
                b"<html><body>Request-FP fixture</body></html>",
                "text/html; charset=utf-8",
            )
        elif path == "/headers":
            self.send_json({"headers": dict(self.headers.items())})
        elif path == "/cookies":
            cookie = SimpleCookie(self.headers.get("Cookie", ""))
            self.send_json(
                {
                    "cookies": {
                        key: morsel.value
                        for key, morsel in cookie.items()
                    }
                }
            )
        else:
            self.send_request_echo()

    def send_upload_summary(self):
        """Multipart upload as sizes and hashes, for large binary files."""
        body = self.read_body()
        message = BytesParser(policy=policy.default).parsebytes(
            (
                "Content-Type: {}\r\nMIME-Version: 1.0\r\n\r\n"
            ).format(self.headers.get("Content-Type", "")).encode("ascii")
            + body
        )
        form, files = {}, {}
        for part in message.iter_parts():
            name = part.get_param("name", header="content-disposition")
            payload = part.get_payload(decode=True)
            if part.get_filename() is None:
                form[name] = payload.decode("utf-8", errors="replace")
            else:
                files[name] = {
                    "filename": part.get_filename(),
                    "size": len(payload),
                    "sha256": hashlib.sha256(payload).hexdigest(),
                    "md5": hashlib.md5(payload).hexdigest(),
                }
        self.send_json(
            {
                "content_length": len(body),
                "transfer_encoding": self.headers.get("Transfer-Encoding", ""),
                "form": form,
                "files": files,
            }
        )

    def send_flaky(self, path):
        """/flaky?key=K&fail=N[&status=503][&retry_after=S][&mode=drop][&delay=S]

        The first N requests for key K fail (with status, or by closing the
        connection without a response when mode=drop); later ones return 200
        with the attempt number. /flaky-count?key=K reports the count.
        Returns True if handled."""
        if path not in ("/flaky", "/flaky-count"):
            return False
        args = single_values(parse_qs(urlsplit(self.path).query))
        key = args.get("key", "")
        self.read_body()
        if path == "/flaky-count":
            with FLAKY_LOCK:
                count = FLAKY_COUNTS.get(key, 0)
            self.send_json({"count": count})
            return True
        with FLAKY_LOCK:
            attempt = FLAKY_COUNTS.get(key, 0) + 1
            FLAKY_COUNTS[key] = attempt
        if attempt <= int(args.get("fail", "0")):
            time.sleep(min(float(args.get("delay", "0")), 10.0))
            if args.get("mode") == "drop":
                self.close_connection = True
                return True
            status = int(args.get("status", "503"))
            body = json.dumps({"attempt": attempt}).encode("utf-8")
            self.send_response(status)
            if "retry_after" in args:
                self.send_header("Retry-After", args["retry_after"])
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            if self.command != "HEAD":
                self.wfile.write(body)
            return True
        body = json.dumps({"attempt": attempt}).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        if self.command != "HEAD":
            self.wfile.write(body)
        return True

    def do_POST(self):
        path = urlsplit(self.path).path
        if self.send_flaky(path):
            pass
        elif path == "/upload-summary":
            self.send_upload_summary()
        elif not self.send_status(path):
            self.send_body_echo()

    def do_PUT(self):
        path = urlsplit(self.path).path
        if not (self.send_flaky(path) or self.send_status(path)):
            self.send_body_echo()

    def do_PATCH(self):
        path = urlsplit(self.path).path
        if not (self.send_flaky(path) or self.send_status(path)):
            self.send_body_echo()

    def do_HEAD(self):
        path = urlsplit(self.path).path
        if self.send_flaky(path):
            return
        status = 200
        if path.startswith("/status/"):
            try:
                status = int(path.rsplit("/", 1)[1])
            except ValueError:
                status = 400
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", "42")
        self.send_header("X-Fixture-Head", "yes")
        self.end_headers()

    def do_DELETE(self):
        path = urlsplit(self.path).path
        if not (self.send_flaky(path) or self.send_status(path)):
            self.send_request_echo()


class FixtureServer(ThreadingHTTPServer):
    def handle_error(self, request, client_address):
        # Clients that time out on purpose close the socket mid-response.
        if isinstance(sys.exc_info()[1], ConnectionError):
            return
        super().handle_error(request, client_address)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=18080)
    args = parser.parse_args()

    server = FixtureServer((args.host, args.port), RequestHandler)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
