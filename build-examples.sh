#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
examples_dir="$repo_root/examples"
output_dir="$repo_root/example-bin"
compiler_output_dir="$output_dir"

if [[ "$output_dir" != "$repo_root/example-bin" ]]; then
  echo "Refusing to clean unexpected output directory: $output_dir" >&2
  exit 1
fi

# Build with lazbuild when available; otherwise (or with --fpc) build each
# project's main source with fpc directly.
use_fpc=false
if [[ "${1:-}" == "--fpc" ]] || ! command -v lazbuild >/dev/null 2>&1; then
  use_fpc=true
fi
if [[ "$use_fpc" == true ]] && ! command -v fpc >/dev/null 2>&1; then
  echo "Neither lazbuild nor fpc was found. Install FPC 3.2.2+ or Lazarus 4.8+." >&2
  exit 1
fi

native_path() {
  case "$(uname -s)" in
    MINGW*|MSYS*|CYGWIN*) cygpath -w "$1" ;;
    *) printf '%s\n' "$1" ;;
  esac
}

compiler_output_dir="$(native_path "$output_dir")"

projects=()
while IFS= read -r project; do
  projects+=("$project")
done < <(
  find "$examples_dir" \
    -type d -name backup -prune -o \
    -type f -name '*.lpi' -print |
    LC_ALL=C sort
)

if [[ ${#projects[@]} -eq 0 ]]; then
  echo "No Lazarus example projects were found under $examples_dir" >&2
  exit 1
fi

rm -rf -- "$output_dir"
mkdir -p -- "$output_dir"

for project in "${projects[@]}"; do
  relative_project="${project#"$repo_root/"}"
  echo "Building $relative_project"

  if [[ "$use_fpc" == true ]]; then
    project_dir="$(dirname -- "$project")"
    main_file="$(grep -oE '<Filename Value="[^"]+\.(lpr|pas)"' "$project" |
      head -n 1 | sed -E 's/.*Value="([^"]+)"/\1/')"
    if [[ -z "$main_file" ]]; then
      echo "Could not find the main source file in $relative_project" >&2
      exit 1
    fi
    unit_dir="$output_dir/.units/$(basename -- "$project" .lpi)"
    mkdir -p -- "$unit_dir"
    fpc -B -MObjFPC -Scghi -O2 -v0 -l- \
      "-Fu$(native_path "$repo_root/src")" \
      "-FU$(native_path "$unit_dir")" \
      "-FE$compiler_output_dir" \
      "$(native_path "$project_dir/$main_file")"
  else
    lazbuild \
      --quiet \
      --quiet \
      --build-all \
      --build-mode=Release \
      --no-write-project \
      "--opt=-FE$compiler_output_dir" \
      "$project"
  fi
done
rm -rf -- "$output_dir/.units"

echo
echo "Built ${#projects[@]} example projects into $output_dir"
while IFS= read -r built_file; do
  echo "  ${built_file##*/}"
done < <(
  find "$output_dir" -maxdepth 1 -type f -print |
    LC_ALL=C sort
)
