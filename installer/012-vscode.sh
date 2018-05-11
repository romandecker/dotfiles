#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

VSCODE_USER_CONFIG_DIR="Library/Application Support/Code/User"

if [ $os == $LINUX ]; then
    VSCODE_USER_CONFIG_DIR=".config/Code/User"
else
    defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false
fi

link_dotfile vscode "$VSCODE_USER_CONFIG_DIR"
