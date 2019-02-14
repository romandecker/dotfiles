#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

link_dotfile dotgitignore .gitignore

echo "Configuring global git excludesfile"
git config --global core.excludesfile $HOME/.gitignore


git config --global alias.co '!git checkout $(git branch | fzf)'
