#!/usr/bin/env bash

os=''
LINUX='linux'
MAC='mac'
DOTFILES_PATH='~/.installed-dotfiles'

function ensure() {
    local package=$1
    local command=$2

    if [ -z "$command" ]; then
        command=$1
    fi

    if [ -x "$(command -v $command)" ]; then
        echo "$command is already installed"
        return
    fi

    echo "$package is not installed, installing... (you might be asked for your password)"
    if [ "$os" == "$LINUX" ]; then
        sudo apt-get install -y $package
    elif [ "$os" == "$MAC" ]; then
        brew install $package
    fi
}


if [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    os=$LINUX
    echo "Detected OS: $os"
else
    os=$MAC
    echo "Detected OS: $os"

    echo "Checking if brew is installed"
    # make sure brew is installed
    if [ -x "$(command -v brew)" ]; then
      /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
fi

ensure git

git clone git@github.com:DeX3/dotfiles.git $DOTFILES_PATH

$DOTFILES_PATH/installer/install.sh
