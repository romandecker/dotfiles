#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

if [ $os == $LINUX ]; then
    ensure i3
    ensure i3blocks
    clone https://github.com/vivien/i3blocks-contrib.git $HOME/.i3blocks-contrib

    mkdir -p ~/.config/i3
    link_dotfile .i3config .config/i3/config
    link_dotfile .i3blocks.conf
fi
