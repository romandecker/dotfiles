#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

section neovim

mkdir -p $HOME/.vim

if [ $os == $LINUX ]; then
    echo "Adding apt repository for neovim"
    ensure software-properties-common
    $sudo add-apt-repository ppa:neovim-ppa/stable
    $sudo apt-get update
    ensure neovim nvim
elif [ $os == $MAC ]; then
    ensure neovim/neovim/neovim nvim
fi

link_dotfile .vimrc
link_dotfile .vimrc.abbreviations
link_dotfile .nvimrc
link_dotfile .vim/ftplugin

echo "Installing vim-plug..."
curl -fLo $HOME/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

nvim -c PlugInstall -c qall
