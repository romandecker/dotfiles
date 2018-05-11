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
if [ -z "$(command -v $sudo)" ]; then
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
      local absolute_target="$target"

    if [[ "${target:0:1}" != "/" ]]; then
      absolute_target="$HOME/$target"
    fi


    local target_parent=$(dirname "$absolute_target")

    local REPLACE_OPTION="Replace with new link to $absolute_source" 
    local SKIP_OPTION="Skip/Ignore"


    if [ -L "$absolute_target" ]; then
        local current_source=$(readlink "$absolute_target")
        if [ "$absolute_source" = "$current_source" ]; then
            cecho "$fawn$source$normal is already linked ($fawn$absolute_target$normal)"
            return 0;
        fi
    fi

    cecho "Linking $fawn$absolute_target$fawn$normal to $fawn$absolute_source$fawn"
    if [ -d "$absolute_target" ]; then
        cecho "$fawn$absolute_target$normal already exists, here's a glimpse:"
        ls -ld "$absolute_target"
        local children=$(ls -lA "$absolute_target/" | sed "s/^/  /")
        if [ $(echo "$children" | wc -l) -gt 10 ]; then
            echo "$children" | head -n 10
            echo "  ..."
        else
            echo "$children"
        fi

        echo "What do you want to do?"
        local options=("$REPLACE_OPTION" "$SKIP_OPTION")
        local skip=""
        select opt in "${options[@]}"
        do
            skip=""
            case $opt in
                "$REPLACE_OPTION")
                    rm -rf "$absolute_target"
                    break;
                    ;;
                "$SKIP_OPTION")
                    skip="true"
                    break;
                    ;;

                *)
                    echo "Invalid choice"
                    ;;
            esac
        done
    fi

    if [ -z "$skip" ]; then
      mkdir -p "$target_parent"
      ln -s "$absolute_source" "$absolute_target"
    fi
}

function clone {
    # get the last passed argument and set it to target, see https://stackoverflow.com/questions/1853946/getting-the-last-argument-passed-to-a-shell-script
    for target; do true; done

    if [ -d $target ]; then
      cecho "$fawn$target$fawn already exists"
      return
    fi

    git clone $@
}

function cecho {
    echo "$1$normal"
}

function section {
    cecho "${red}## ${blue}$1"
}
