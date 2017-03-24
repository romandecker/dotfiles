#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

echo "######## fonts #######"

pushd $SCRIPT_DIR../submodules/powerline-fonts
./install.sh
popd
