#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

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

mkdir -p $HOME/.vim/backup

mkdir -p $HOME/.config/nvim
ln -f -s $HOME/.nvimrc $HOME/.config/nvim/init.vim

if ! [ -d $HOME/.local/share/nvim/site/autoload/plug.vim ]; then
    echo "Installing vim-plug..."
    curl -fLo $HOME/.local/share/nvim/site/autoload/plug.vim --create-dirs \
         https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

command -v pip2 && pip2 install --user neovim

command -v pip3 && pip3 install --user neovim

echo "Installing plugins..."
nvim -c PlugInstall -c qall
