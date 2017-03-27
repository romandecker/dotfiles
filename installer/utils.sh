#!/usr/bin/env bash

set -e

os=''
LINUX='linux'
MAC='mac'
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

#normal=$(tput sgr0)                      # normal text
normal=$'\e[0m'                           # (works better sometimes)
bold=$(tput bold)                         # make colors bold/bright
red="$bold$(tput setaf 1)"                # bright red text
green=$(tput setaf 2)                     # dim green text
fawn=$(tput setaf 3); beige="$fawn"       # dark yellow text
yellow="$bold$fawn"                       # bright yellow text
darkblue=$(tput setaf 4)                  # dim blue text
blue="$bold$darkblue"                     # bright blue text
purple=$(tput setaf 5); magenta="$purple" # magenta text
pink="$bold$purple"                       # bright magenta text
darkcyan=$(tput setaf 6)                  # dim cyan text
cyan="$bold$darkcyan"                     # bright cyan text
gray=$(tput setaf 7)                      # dim white text
darkgray="$bold"$(tput setaf 0)           # bold black = dark gray text
white="$bold$gray"                        # bright white text

if [ "$(uname)" == "Darwin" ]; then
    os=$MAC

    # make sure brew is installed
    if ! [ -x "$(command -v brew)" ]; then
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    os=$LINUX
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
        cecho "$fawn$command$normal is already installed"
        return
    fi

    echo "$fawn$package$normal is not installed, installing... (you might be asked for your password)"
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

    cecho "Linking $fawn$absolute_target$fawn$normal to $fawn$absolute_source$fawn"
    ln -s -i $absolute_source $absolute_target
}

function clone {
    if [ -d $2 ]; then
      echo "$fawn$2$fawn already exists"
      return
    fi

    git clone $1 $2
}

function cecho {
    echo "$1$normal"
}

function section {
    cecho "${red}## ${blue}$1"
}
