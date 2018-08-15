#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

sudo='sudo'
if [ -z "$(command -v $sudo)" ]; then
    sudo=''
fi

if [ -x "$(command -v wakatime)" ]; then
    $sudo pip install wakatime
fi
