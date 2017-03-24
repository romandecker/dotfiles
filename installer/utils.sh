#!/usr/bin/env bash

set -e

os=''
LINUX='linux'
MAC='mac'
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    os=$LINUX
else
    os=$MAC

    # make sure brew is installed
    if [ -x "$(command -v brew)" ]; then
      /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
fi

sudo='sudo'
if ! [ -z "command -v $sudo" ]; then
    sudo=''
fi

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
        # sudo apt-get install -y $package
        $sudo apt-get install -y $package
    elif [ "$os" == "$MAC" ]; then
        brew install $package
    fi
}

function abs_path {
    echo "$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"
}

function link_dotfile() {
    local source=$1
    local target=$2

    if [ -z "$target" ]; then
        target="$1"
    fi

    local absolute_source=$(abs_path "$SCRIPT_DIR/../$source")
    local absolute_target=$(abs_path "$HOME/$target")

    echo "Linking $absolute_target to $absolute_source"
    ln -s -i $absolute_source $absolute_target
}

function clone {
    if [ -d $2 ]; then
      echo "$2 already exists"
      return
    fi

    git clone $1 $2
}
