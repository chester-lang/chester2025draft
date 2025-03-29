#!/usr/bin/env bash

# debug-flags.sh - A utility script to manage Chester debug flags
# Usage: source ./debug-flags.sh

# Define all available debug flags
declare -a CHESTER_DEBUG_FLAGS=(
  "ENV_DEBUG"
  "ENV_DEBUG_UNION_SUBTYPING"
  "ENV_DEBUG_UNION_MATCHING"
  "ENV_DEBUG_LITERALS"
  "ENV_DEBUG_IDENTIFIERS"
  "ENV_DEBUG_METHOD_CALLS"
  "ENV_DEBUG_STRING_ARGS"
)

# Colors for terminal output
CHESTER_RED='\033[0;31m'
CHESTER_GREEN='\033[0;32m'
CHESTER_YELLOW='\033[0;33m'
CHESTER_BLUE='\033[0;34m'
CHESTER_NC='\033[0m' # No Color

# Check if script is being sourced
(return 0 2>/dev/null) && CHESTER_SOURCED=1 || CHESTER_SOURCED=0

# Print help message
chester_debug_help() {
  echo -e "${CHESTER_BLUE}Chester Debug Flags Manager${CHESTER_NC}"
  echo ""
  echo "Usage:"
  echo "  source ./debug-flags.sh                 # Load this script"
  echo "  enable_debug FLAG1 [FLAG2...]           # Enable specific debug flags"
  echo "  disable_debug FLAG1 [FLAG2...]          # Disable specific debug flags"
  echo "  enable_all_debug                        # Enable all debug flags"
  echo "  disable_all_debug                       # Disable all debug flags"
  echo "  show_debug_status                       # Show current debug flags status"
  echo "  list_debug_flags                        # List all available debug flags"
  echo ""
  echo "Available flags:"
  for flag in "${CHESTER_DEBUG_FLAGS[@]}"; do
    echo "  $flag"
  done
  echo ""
  echo "Examples:"
  echo "  source ./debug-flags.sh"
  echo "  enable_debug ENV_DEBUG_UNION_MATCHING"
  echo "  enable_all_debug"
  echo "  show_debug_status"
}

# Check debug flag validity
is_valid_debug_flag() {
  local flag="$1"
  for available_flag in "${CHESTER_DEBUG_FLAGS[@]}"; do
    if [ "$flag" = "$available_flag" ]; then
      return 0
    fi
  done
  return 1
}

# Enable specified debug flags
enable_debug() {
  if [ $# -eq 0 ]; then
    echo -e "${CHESTER_RED}Error:${CHESTER_NC} No flags specified"
    echo "Usage: enable_debug FLAG1 [FLAG2...]"
    return 1
  fi

  for flag in "$@"; do
    if is_valid_debug_flag "$flag"; then
      export "$flag=true"
      echo -e "${CHESTER_GREEN}Enabled${CHESTER_NC} $flag"
    else
      echo -e "${CHESTER_RED}Invalid flag:${CHESTER_NC} $flag"
      echo "Run list_debug_flags to see available flags"
    fi
  done
}

# Disable specified debug flags
disable_debug() {
  if [ $# -eq 0 ]; then
    echo -e "${CHESTER_RED}Error:${CHESTER_NC} No flags specified"
    echo "Usage: disable_debug FLAG1 [FLAG2...]"
    return 1
  fi

  for flag in "$@"; do
    if is_valid_debug_flag "$flag"; then
      unset "$flag"
      echo -e "${CHESTER_YELLOW}Disabled${CHESTER_NC} $flag"
    else
      echo -e "${CHESTER_RED}Invalid flag:${CHESTER_NC} $flag"
      echo "Run list_debug_flags to see available flags"
    fi
  done
}

# Enable all debug flags
enable_all_debug() {
  for flag in "${CHESTER_DEBUG_FLAGS[@]}"; do
    export "$flag=true"
  done
  echo -e "${CHESTER_GREEN}Enabled all debug flags${CHESTER_NC}"
}

# Disable all debug flags
disable_all_debug() {
  for flag in "${CHESTER_DEBUG_FLAGS[@]}"; do
    unset "$flag"
  done
  echo -e "${CHESTER_YELLOW}Disabled all debug flags${CHESTER_NC}"
}

# Show status of all debug flags
show_debug_status() {
  echo -e "${CHESTER_BLUE}Debug Flag Status:${CHESTER_NC}"
  for flag in "${CHESTER_DEBUG_FLAGS[@]}"; do
    if [ -n "${!flag}" ]; then
      echo -e "  ${CHESTER_GREEN}✓${CHESTER_NC} $flag: ${CHESTER_GREEN}enabled${CHESTER_NC}"
    else
      echo -e "  ${CHESTER_RED}✗${CHESTER_NC} $flag: ${CHESTER_RED}disabled${CHESTER_NC}"
    fi
  done
}

# List all available flags
list_debug_flags() {
  echo -e "${CHESTER_BLUE}Available debug flags:${CHESTER_NC}"
  for flag in "${CHESTER_DEBUG_FLAGS[@]}"; do
    echo "  $flag"
  done
}

# Print a reminder message if the script is executed directly
if [ $CHESTER_SOURCED -eq 0 ]; then
  echo -e "${CHESTER_YELLOW}Note:${CHESTER_NC} This script should be sourced, not executed."
  echo -e "Please use: ${CHESTER_BLUE}source ./debug-flags.sh${CHESTER_NC}"
  echo -e "Run ${CHESTER_BLUE}source ./debug-flags.sh && chester_debug_help${CHESTER_NC} for more information."
  exit 1
fi

# Print initial message
if [ $CHESTER_SOURCED -eq 1 ]; then
  echo -e "${CHESTER_GREEN}Chester debug flags manager loaded${CHESTER_NC}"
  echo -e "Run ${CHESTER_BLUE}chester_debug_help${CHESTER_NC} for usage information"
fi 