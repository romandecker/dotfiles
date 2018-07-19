#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

if [ ! -x "$(command -v pip)" ]; then
    sudo easy_install pip
fi
