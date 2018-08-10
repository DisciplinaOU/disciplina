#!/usr/bin/env bash
set -euo pipefail
if [ $# -lt 1 ]; then
  echo "Usage: deploy-result.sh unit-name [output-name]"
  exit 1
fi
UNIT=$1
OUTPUT=$UNIT
if [ $# -gt 1 ]; then
  OUTPUT=$2
fi
RESTART=false
echo "Checking current status"
if ! systemctl is-active "$UNIT" --quiet; then
  echo "Unit currently inactive"
  RESTART=true
fi
NEW_PATH=$(buildkite-agent meta-data get output-"$OUTPUT")
TARGET=/var/lib/buildkite-agent/cd/"$UNIT"
if [ ! -e "$TARGET" ] || [ "$(readlink "$TARGET")" != "$NEW_PATH" ]; then
  RESTART=true
  echo "--- Downloading and updating symlink"
  nix-store --realise "$NEW_PATH" --indirect --add-root "$TARGET"
else
  echo "No update needed"
fi
rev=$(git rev-parse --short HEAD)
github_url=${BUILDKITE_REPO%.git}/commit/$rev
if [ "$RESTART" = true ]; then
  echo "+++ Restarting service"
  /run/wrappers/bin/sudo systemctl restart "$UNIT"
  sleep 15s
  systemctl status "$UNIT" --no-pager -l
  systemctl is-active "$UNIT" --quiet
  message="Build <https://ci-gate.serokell.io/buildkite_public_log?$BUILDKITE_BUILD_URL|#$BUILDKITE_BUILD_NUMBER>: updated $UNIT to <$github_url|$rev> :tada:"
  echo "$message"
else
  message="Build <https://ci-gate.serokell.io/buildkite_public_log?$BUILDKITE_BUILD_URL|#$BUILDKITE_BUILD_NUMBER>: $UNIT unchanged to <$github_url|$rev> :shrug:"
fi
curl -X POST --data-urlencode "payload={\"text\": \"$message\"}" "$SECRET_SLACK_HOOK"

