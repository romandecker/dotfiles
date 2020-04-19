#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh


DOOM_INSTALLATION_DIR=$SCRIPT_DIR/../doom-emacs

[[ -d $DOOM_INSTALLATION_DIR ]] || git clone git@github.com:hlissner/doom-emacs.git $DOOM_INSTALLATION_DIR
(cd $DOOM_INSTALLATION_DIR && git pull)

$DOOM_INSTALLATION_DIR/bin/doom install

$SCRIPT_DIR/../submodules/chemacs/install.sh
rm -f $HOME/.emacs-profiles.el
link_dotfile .emacs-profiles.el
link_dotfile .doom.d

