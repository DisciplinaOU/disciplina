#!/usr/bin/env bash
echo "--- Uploading result metadata"
files=$(find . -maxdepth 1 -type l -name 'result*')
if [ -z "$files" ]; then
  echo "Nothing to upload :wave:"
  exit 0
fi
for output in $files; do
  # get nix name from path
  name=$(nix eval '(with builtins; (parseDrvName (unsafeDiscardStringContext (substring 33 (-1) (baseNameOf (storePath '"$output"'))))).name)' --raw)
  buildkite-agent meta-data set "output-$name" "$(readlink "$output")";
  echo "output-$name" "->" "$(readlink "$output")"
done

