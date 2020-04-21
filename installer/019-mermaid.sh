#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh


if [ -x "$(command -v mmdc)" ]; then
    cecho "$fawn" "mmdc$normal is already installed"
else
    cecho "$fawn" "mmdc$normal is not installed. Please run 'npm install -g mermaid.cli'"
fi
