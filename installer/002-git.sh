#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

echo "######### git ########"

link_dotfile dotgitignore .gitignore

echo "Configuring global git excludesfile"
git config --global core.excludesfile $HOME/.gitignore
