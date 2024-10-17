#!/usr/bin/env bash

set -e

curl -fsSL https://moonrepo.dev/install/proto.sh | bash

proto plugin add --global chester "https://github.com/chester-lang/chester/raw/refs/heads/main/proto.toml"

proto install chester