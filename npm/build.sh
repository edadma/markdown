#!/usr/bin/env bash
#
# Bundle the npm package by re-linking the Scala.js output and copying the
# minified `main.js` (plus LICENSE) into this directory. Run from the
# repo root *or* from npm/.  The published artifact is whatever ends up in
# this directory; `npm publish` from here.
#
# Usage:
#   ./npm/build.sh             # link in fullOpt mode (release)
#   ./npm/build.sh fast        # link in fastOpt mode (faster, larger)
#
set -euo pipefail

# Locate repo root regardless of where the script is invoked from.
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"

mode="${1:-full}"

case "$mode" in
  full)
    sbt_task="markdownJS/fullLinkJS"
    out_subdir="markdown-opt"
    ;;
  fast)
    sbt_task="markdownJS/fastLinkJS"
    out_subdir="markdown-fastopt"
    ;;
  *)
    echo "Usage: $0 [full|fast]" >&2
    exit 1
    ;;
esac

echo ">>> linking ($mode) ..."
( cd "$repo_root" && sbt -batch "$sbt_task" )

linked="$repo_root/js/target/scala-3.8.4/$out_subdir/main.js"
if [[ ! -f "$linked" ]]; then
  echo "linked output not found: $linked" >&2
  exit 1
fi

echo ">>> copying main.js → npm/main.js"
cp "$linked" "$script_dir/main.js"

echo ">>> copying LICENSE → npm/LICENSE"
cp "$repo_root/LICENSE" "$script_dir/LICENSE"

echo ">>> done. To publish: cd npm && npm publish"
