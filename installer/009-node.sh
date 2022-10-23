#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

# Note that nvm is now managed via the lukechilds/zsh-nvm zsh-plugin

if [ -x "$(command -v yvm)" ]; then
    cecho "$fawn$" yvm "$normal is already installed"
else
    curl -fsSL https://raw.githubusercontent.com/tophat/yvm/master/scripts/install.sh | bash
fi

