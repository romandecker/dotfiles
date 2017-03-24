#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

echo "######## fonts #######"

FONTS_DIR=''
if [ $os == $LINUX ]; then
    FONTS_DIR='.fonts'
elif [ $os == $MAC ]; then
    FONTS_DIR='Library/fonts'
fi

if [ -f "$HOME/$FONTS_DIR" ]; then
    echo "Fonts already installed"
    return
fi

clone https://github.com/powerline/fonts.git /tmp/powerline-fonts

pushd /tmp/powerline-fonts
./install.sh
popd

rm -rf /tmp/powerline-fonts
