#!/usr/bin/env python3
"""Small httpbin-compatible fixture for Request-FP's integration tests."""

import argparse
import json
from email import policy
from email.parser import BytesParser
from http.cookies import SimpleCookie
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlsplit


def single_values(values):
    return {key: items[-1] for key, items in values.items()}


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
        else:
            form = single_values(
                parse_qs(
                    body.decode("utf-8"),
                    keep_blank_values=True,
                    encoding="utf-8",
                )
            )

        return form, files, json_body

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
        form, files, json_body = self.parse_body(body)
        details = self.request_details()
        details.update(
            {
                "data": body.decode("utf-8", errors="replace"),
                "files": files,
                "form": form,
                "json": json_body,
            }
        )
        self.send_json(details)

    def do_GET(self):
        path = urlsplit(self.path).path

        if path == "/health":
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
        elif path.startswith("/status/"):
            try:
                status = int(path.rsplit("/", 1)[1])
            except ValueError:
                status = 400
            self.send_json({"status": status}, status)
        else:
            self.send_request_echo()

    def do_POST(self):
        self.send_body_echo()

    def do_PUT(self):
        self.send_body_echo()

    def do_DELETE(self):
        self.send_request_echo()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=18080)
    args = parser.parse_args()

    server = ThreadingHTTPServer((args.host, args.port), RequestHandler)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
