#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

if [ -x "$(command -v grip)" ]; then
        cecho "$fawn""grip""$normal is already installed"
else
    pip3 install grip
fi
