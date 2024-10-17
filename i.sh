#!/usr/bin/env bash

set -e

check_cmd() {
  command -v "$1" > /dev/null 2>&1
  return $?
}
proto_cmd=proto
if ! check_cmd proto; then
  curl -fsSL https://moonrepo.dev/install/proto.sh | bash
  proto_cmd="$HOME/.proto/bin/proto"
  if ! check_cmd "$proto_cmd"; then
    echo "proto command not found"
    exit 1
  fi
fi

"$proto_cmd" plugin add --global chester "https://github.com/chester-lang/chester/raw/refs/heads/main/proto.toml"

exec "$proto_cmd" install chester