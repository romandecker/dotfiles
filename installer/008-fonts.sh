#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

pushd $SCRIPT_DIR/../submodules/powerline-fonts
./install.sh
popd


FONT_ZIP_URLS=(
    "https://github.com/tonsky/FiraCode/releases/download/5.2/Fira_Code_v5.2.zip"
    "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/Agave.zip"
)

if [ $os == $MAC ]; then
    # Copy fonts into macOS fonts dir
    for url in "${FONT_ZIP_URLS[@]}"; do
        echo "Installing fonts from $url..."
        TMPFILE=$(mktemp)
        curl -L $url -o $TMPFILE
        TMPDIR=$(mktemp -d)
        unzip $TMPFILE -d $TMPDIR
        cp $TMPDIR/**/*.ttf ~/Library/Fonts/ || true
        cp $TMPDIR/*.ttf ~/Library/Fonts/ || true
    done <<< $FONT_ZIP_URLS
fi