#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

pushd $SCRIPT_DIR/../submodules/powerline-fonts
./install.sh
popd

TMPFILE=$(mktemp)

# Download and unzip FiraCode
curl -L https://github.com/tonsky/FiraCode/releases/latest/download/FiraCode_2.zip -o $TMPFILE
TMPDIR=$(mktemp -d)
unzip $TMPFILE -d $TMPDIR

if [ $os == $MAC ]; then
    # Copy fonts into macOS fonts dir
    cp $TMPDIR/ttf/*.ttf ~/Library/Fonts/
fi
