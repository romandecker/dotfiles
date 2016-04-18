#!/bin/sh

mkdir -p ~/.vim/backup
mkdir -p ~/.vim/tmp

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

curl -fLo ~/.vimrc \
  https://raw.githubusercontent.com/DeX3/dotfiles/master/server.vimrc

vim -c PlugInstall -c qall
