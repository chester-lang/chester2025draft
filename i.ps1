#!/usr/bin/env pwsh

irm https://moonrepo.dev/install/proto.ps1 | iex

proto plugin add --global chester "https://github.com/chester-lang/chester/raw/refs/heads/main/proto.toml"

proto install chester