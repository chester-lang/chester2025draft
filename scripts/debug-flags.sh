#!/usr/bin/env bash

# debug-flags.sh - A utility script to manage Chester debug flags
# Usage: ./scripts/debug-flags.sh [command] [flags...]

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Define all available debug flags
ALL_FLAGS=(
  "ENV_DEBUG"
  "ENV_DEBUG_UNION_SUBTYPING"
  "ENV_DEBUG_UNION_MATCHING"
  "ENV_DEBUG_LITERALS"
  "ENV_DEBUG_IDENTIFIERS"
  "ENV_DEBUG_METHOD_CALLS"
  "ENV_DEBUG_STRING_ARGS"
)

# Print usage information
usage() {
  echo -e "${BLUE}Chester Debug Flags Manager${NC}"
  echo ""
  echo "Usage: $0 <command> [flags...]"
  echo ""
  echo "Commands:"
  echo "  enable FLAG1 [FLAG2...]   Enable specified debug flags"
  echo "  disable FLAG1 [FLAG2...]  Disable specified debug flags"
  echo "  enable-all                Enable all debug flags"
  echo "  disable-all               Disable all debug flags"
  echo "  status                    Show status of all debug flags"
  echo "  list                      List all available debug flags"
  echo ""
  echo "Available flags:"
  for flag in "${ALL_FLAGS[@]}"; do
    echo "  $flag"
  done
  echo ""
  echo "Examples:"
  echo "  $0 enable ENV_DEBUG_UNION_MATCHING"
  echo "  $0 disable-all"
  echo "  $0 enable ENV_DEBUG"
  echo "  $0 status"
  exit 1
}

# Check flag validity
is_valid_flag() {
  local flag="$1"
  for available_flag in "${ALL_FLAGS[@]}"; do
    if [ "$flag" = "$available_flag" ]; then
      return 0
    fi
  done
  return 1
}

# Enable specified debug flags
enable_flags() {
  for flag in "$@"; do
    if is_valid_flag "$flag"; then
      export "$flag=true"
      echo -e "${GREEN}Enabled${NC} $flag"
    else
      echo -e "${RED}Invalid flag:${NC} $flag"
      echo "Run '$0 list' to see available flags"
    fi
  done
}

# Disable specified debug flags
disable_flags() {
  for flag in "$@"; do
    if is_valid_flag "$flag"; then
      unset "$flag"
      echo -e "${YELLOW}Disabled${NC} $flag"
    else
      echo -e "${RED}Invalid flag:${NC} $flag"
      echo "Run '$0 list' to see available flags"
    fi
  done
}

# Enable all debug flags
enable_all_flags() {
  for flag in "${ALL_FLAGS[@]}"; do
    export "$flag=true"
  done
  echo -e "${GREEN}Enabled all debug flags${NC}"
}

# Disable all debug flags
disable_all_flags() {
  for flag in "${ALL_FLAGS[@]}"; do
    unset "$flag"
  done
  echo -e "${YELLOW}Disabled all debug flags${NC}"
}

# Show status of all debug flags
show_status() {
  echo -e "${BLUE}Debug Flag Status:${NC}"
  for flag in "${ALL_FLAGS[@]}"; do
    if [ -n "${!flag}" ]; then
      echo -e "  ${GREEN}✓${NC} $flag: ${GREEN}enabled${NC}"
    else
      echo -e "  ${RED}✗${NC} $flag: ${RED}disabled${NC}"
    fi
  done
}

# List all available flags
list_flags() {
  echo -e "${BLUE}Available debug flags:${NC}"
  for flag in "${ALL_FLAGS[@]}"; do
    echo "  $flag"
  done
}

# Main command handler
if [ $# -lt 1 ]; then
  usage
fi

case "$1" in
  "enable")
    if [ $# -lt 2 ]; then
      echo -e "${RED}Error:${NC} No flags specified to enable"
      echo "Usage: $0 enable FLAG1 [FLAG2...]"
      exit 1
    fi
    shift
    enable_flags "$@"
    ;;
  "disable")
    if [ $# -lt 2 ]; then
      echo -e "${RED}Error:${NC} No flags specified to disable"
      echo "Usage: $0 disable FLAG1 [FLAG2...]"
      exit 1
    fi
    shift
    disable_flags "$@"
    ;;
  "enable-all")
    enable_all_flags
    ;;
  "disable-all")
    disable_all_flags
    ;;
  "status")
    show_status
    ;;
  "list")
    list_flags
    ;;
  *)
    echo -e "${RED}Error:${NC} Unknown command: $1"
    usage
    ;;
esac

cat << EOF

${BLUE}Note:${NC} Debug flags are set for this terminal session only.
To make them persist, add export commands to your shell profile.

For example:
  export ENV_DEBUG=true

To run a command with debug flags temporarily:
  ENV_DEBUG=true sbt rootJVM/test

EOF 