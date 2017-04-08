#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

ensure tmux

clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm

link_dotfile .tmux.conf
link_dotfile .tmux.conf.macosx
