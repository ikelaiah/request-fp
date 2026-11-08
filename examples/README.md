# Request-FP examples

Each subdirectory contains a Lazarus project demonstrating one Request-FP
task. The examples call `https://httpbin.org`, so running most of them requires
network access.

## Build all examples

From the repository root, use the script for your shell.

Windows PowerShell:

```powershell
.\build-examples.ps1
```

Linux or Git Bash:

```bash
bash ./build-examples.sh
```

The scripts:

- use Lazarus 4.8+ (`lazbuild` on `PATH`); `build-examples.sh` falls back to
  plain `fpc` without it (`--fpc` forces this);
- find every `.lpi` project below this directory;
- ignore Lazarus `backup/` directories;
- clean the root-level `example-bin/` directory;
- compile each project in Release mode; and
- place all generated executables in `example-bin/`.

On Windows:

```powershell
.\example-bin\easy_get.exe
```

On Linux:

```bash
./example-bin/easy_get
```

## Included examples

- `easy_get` — one-off GET request
- `custom_headers_params` — request headers and query parameters
- `basic_auth` — HTTP Basic authentication
- `post_json` — posting and reading JSON
- `multipart_upload` — multipart fields and files
- `file_download` — saving a response to disk
- `streaming_download` — large download streamed to disk with a progress bar
- `download_with_redirects` — redirect handling
- `retry_on_error` — automatic retries, and a manual retry for POST
- `typed_errors` — telling timeouts, network failures, and HTTP errors apart
- `session_easy_get` — minimal session usage
- `session_cookie_headers` — persistent cookies and headers
- `session_login_auth` — JSON login and authenticated follow-up request
- `session_login_redirect` — login cookie kept across a redirect, bearer token
- `session_put_delete_json` — session PUT and DELETE requests
- `ssl_debug` — OpenSSL architecture and DLL diagnostics
