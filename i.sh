#!/usr/bin/env bash

set -e

check_cmd() {
  command -v "$1" > /dev/null 2>&1
  return $?
}

if [[ -z "$PROTO_HOME" ]]; then
  install_dir="$HOME/.proto/bin"
else
  install_dir="$PROTO_HOME/bin"
fi

if [[ "$OS" == "Windows_NT" ]]; then
  proto_exe="proto.exe"
else
  proto_exe="proto"
fi

if check_cmd "$proto_exe"; then
  proto_cmd="$proto_exe"
else
  proto_cmd="$install_dir/$proto_exe"
  curl -fsSL https://moonrepo.dev/install/proto.sh | bash
  if ! check_cmd "$proto_cmd"; then
    echo "proto command not found"
    exit 1
  fi
fi

"$proto_cmd" plugin add --global chester "https://github.com/chester-lang/chester/raw/refs/heads/main/proto.toml"

exec "$proto_cmd" install chester