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

if ! command -v lazbuild >/dev/null 2>&1; then
  echo "lazbuild was not found. Install Lazarus 4.8+ and add lazbuild to PATH." >&2
  exit 1
fi

case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*)
    compiler_output_dir="$(cygpath -w "$output_dir")"
    ;;
esac

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

  lazbuild \
    --quiet \
    --quiet \
    --build-all \
    --build-mode=Release \
    --no-write-project \
    "--opt=-FE$compiler_output_dir" \
    "$project"
done

echo
echo "Built ${#projects[@]} example projects into $output_dir"
while IFS= read -r built_file; do
  echo "  ${built_file##*/}"
done < <(
  find "$output_dir" -maxdepth 1 -type f -print |
    LC_ALL=C sort
)
