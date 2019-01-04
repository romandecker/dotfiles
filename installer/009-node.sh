#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

NVM_DIR="$HOME/.nvm"

clone https://github.com/creationix/nvm.git $NVM_DIR
pushd "$NVM_DIR"
git checkout `git describe --abbrev=0 --tags --match "v[0-9]*" origin`
popd

if [ -x "$(command -v yvm)" ]; then
    cecho "$fawn$command$normal is already installed"
else
    curl -fsSL https://raw.githubusercontent.com/tophat/yvm/master/scripts/install.sh | bash
fi
