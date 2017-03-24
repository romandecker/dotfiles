#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

echo "#######  zsh  ########"

ensure zsh

link_dotfile .zshrc

echo "Setting zsh as your default shelL"
chsh -s $(which zsh)
