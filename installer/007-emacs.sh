#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh


$SCRIPT_DIR/../submodules/chemacs/install.sh
rm -f $HOME/.emacs-profiles.el
link_dotfile .emacs-profiles.el
link_dotfile .doom.d
