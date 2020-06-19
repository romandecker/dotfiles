#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh


if [ -z "$(command -v pet)" ]; then
    cecho "$fawn" "pet" "$normal is already installed"
else
    if [ "$(uname)" == "Darwin" ]; then
        ensure knqyf263/pet/pet pet
    else
        echo "Don't know how to install pet on ${uname} yet, please install manually: https://github.com/knqyf263/pet#installation"
    fi
fi

if [ ! -L "$HOME/.config/pet/snippet.toml" ]; then
    echo "Removing regular snippet file"
    rm $HOME/.config/pet/snippet.toml
fi

link_dotfile snippets/pet/snippet.toml .config/pet/snippet.toml
