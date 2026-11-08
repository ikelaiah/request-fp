#!/usr/bin/env bash
# Build and run the Request-FP test suite against the local HTTP fixture.
#
# Usage: ./run-tests.sh [--leak-check] [--fpc] [--port N] [-- TestRunner args...]
#
#   --leak-check  Build the Debug mode (heaptrc, range/overflow checks) and
#                 fail if any memory is left unfreed.
#   --fpc         Build with fpc directly (used automatically without lazbuild).
#   --port N      Fixture port (default 18080).
#
# Set REQUEST_FP_TEST_BASE_URL to test against another server (for example
# https://httpbin.org); the local fixture is then not started.

set -euo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
build_mode=Release
leak_check=false
use_fpc=false
port=18080
runner_args=(-a --format=plain)

while [[ $# -gt 0 ]]; do
  case "$1" in
    --leak-check)
      leak_check=true
      build_mode=Debug
      ;;
    --fpc)
      use_fpc=true
      ;;
    --port)
      port="$2"
      shift
      ;;
    --)
      shift
      if [[ $# -gt 0 ]]; then
        runner_args=("$@")
      fi
      break
      ;;
    -h|--help)
      sed -n '2,13p' "$0"
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      exit 2
      ;;
  esac
  shift
done

if [[ "$use_fpc" != true ]] && ! command -v lazbuild >/dev/null 2>&1; then
  use_fpc=true
fi
if [[ "$use_fpc" == true ]] && ! command -v fpc >/dev/null 2>&1; then
  echo "Neither lazbuild nor fpc was found. Install FPC 3.2.2+ or Lazarus 4.8+." >&2
  exit 1
fi

python=""
for candidate in python3 python; do
  if command -v "$candidate" >/dev/null 2>&1 &&
     "$candidate" -c 'import sys' >/dev/null 2>&1; then
    python="$candidate"
    break
  fi
done
if [[ -z "$python" ]]; then
  echo "Python 3 was not found; it is needed for tests/http_fixture.py." >&2
  exit 1
fi

native_path() {
  if command -v cygpath >/dev/null 2>&1; then
    cygpath -w "$1"
  else
    printf '%s\n' "$1"
  fi
}

runner="$repo_root/tests/TestRunner"

if [[ "$use_fpc" == true ]]; then
  target_os="$(fpc -iTO)"
  case "$target_os" in
    win32|win64) runner="$runner.exe" ;;
  esac
  unit_dir="$repo_root/tests/lib/fpc-$(fpc -iTP)-$target_os-$build_mode"
  mkdir -p -- "$unit_dir"
  # Mirrors the Release and Debug modes in tests/TestRunner.lpi.
  fpc_flags=(-B -MObjFPC -Scghi -FcUTF8 -v0 -l-
    "-Fu$(native_path "$repo_root/src")"
    "-Fu$(native_path "$repo_root/tests")"
    "-FU$(native_path "$unit_dir")"
    "-o$(native_path "$runner")")
  if [[ "$build_mode" == Debug ]]; then
    fpc_flags+=(-gh -gl -gw3 -gt -Ci -Cr -Co -Ct -Sa)
  else
    fpc_flags+=(-O2)
  fi
  echo "Building tests/TestRunner.lpr with fpc ($build_mode)"
  fpc "${fpc_flags[@]}" "$(native_path "$repo_root/tests/TestRunner.lpr")"
else
  echo "Building tests/TestRunner.lpi ($build_mode)"
  lazbuild --quiet --quiet --build-all "--build-mode=$build_mode" \
    --no-write-project "$repo_root/tests/TestRunner.lpi"
  if [[ -f "$runner.exe" ]]; then
    runner="$runner.exe"
  fi
fi

fixture_pid=""
work_dir="$(mktemp -d)"
cleanup() {
  if [[ -n "$fixture_pid" ]]; then
    kill "$fixture_pid" 2>/dev/null || true
    wait "$fixture_pid" 2>/dev/null || true
  fi
  rm -rf -- "$work_dir"
}
trap cleanup EXIT

if [[ -z "${REQUEST_FP_TEST_BASE_URL:-}" ]]; then
  base_url="http://127.0.0.1:$port"
  "$python" "$repo_root/tests/http_fixture.py" --port "$port" &
  fixture_pid="$!"

  ready=false
  for _ in $(seq 1 100); do
    if "$python" -c 'import sys, urllib.request; urllib.request.urlopen(sys.argv[1], timeout=1)' \
        "$base_url/health" >/dev/null 2>&1; then
      ready=true
      break
    fi
    if ! kill -0 "$fixture_pid" 2>/dev/null; then
      break
    fi
    sleep 0.1
  done
  if [[ "$ready" != true ]]; then
    echo "Local HTTP fixture failed to start on port $port." >&2
    exit 1
  fi
  export REQUEST_FP_TEST_BASE_URL="$base_url"
fi
echo "Testing against $REQUEST_FP_TEST_BASE_URL"

heap_log="$work_dir/heaptrc.log"
if [[ "$leak_check" == true ]]; then
  export HEAPTRC="log=$(native_path "$heap_log")"
fi

set +e
"$runner" "${runner_args[@]}"
status=$?
set -e

if [[ "$leak_check" == true ]]; then
  unset HEAPTRC
  if [[ ! -f "$heap_log" ]]; then
    echo "Leak check failed: no heaptrc report was written." >&2
    exit 1
  fi
  unfreed="$(grep -Eo '^[0-9]+ unfreed memory blocks' "$heap_log" | head -n 1 | cut -d' ' -f1)"
  if [[ -z "$unfreed" ]]; then
    echo "Leak check failed: could not read the heaptrc report." >&2
    cat "$heap_log" >&2
    exit 1
  fi
  if [[ "$unfreed" -ne 0 ]]; then
    echo "Leak check failed: $unfreed unfreed memory blocks." >&2
    cat "$heap_log" >&2
    exit 1
  fi
  echo "Leak check passed: 0 unfreed memory blocks."
fi

exit "$status"
