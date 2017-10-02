#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

clone --depth 1 https://github.com/junegunn/fzf.git $HOME/.fzf

$HOME/.fzf/install --bin
