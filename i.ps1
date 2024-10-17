#!/usr/bin/env pwsh

$InstallDir = if ($env:PROTO_HOME) {
  "$($env:PROTO_HOME)\bin"
} else {
  "${Home}\.proto\bin"
}

$BinPath = "${InstallDir}\proto.exe"
if (!(Test-Path $BinPath)) {
  irm https://moonrepo.dev/install/proto.ps1 | iex
}

& "$BinPath" plugin add --global chester "https://github.com/chester-lang/chester/raw/refs/heads/main/proto.toml"

& "$BinPath" install chester