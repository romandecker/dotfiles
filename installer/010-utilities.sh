#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

if [ $os == $LINUX ]; then
    ensure silversearcher-ag ag
elif [ $os == $MAC ]; then
    ensure the_silver_searcher ag
    ensure coreutils realpath
fi

ensure ripgrep
ensure moreutils vipe
ensure watch

ensure highlight
