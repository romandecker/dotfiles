#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/../utils.sh

(brew cask info qlstephen | grep -Fxq "Not installed") && brew install homebrew/cask/qlstephen
