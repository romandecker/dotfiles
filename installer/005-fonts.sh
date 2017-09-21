#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

pushd $SCRIPT_DIR/../submodules/powerline-fonts
./install.sh
popd

FONT_WORKDIR=/tmp/fonts-$(date "+%s")
mkdir -p $FONT_WORKDIR

pushd $FONT_WORKDIR
curl http://fontawesome.io/assets/font-awesome-4.7.0.zip > fontawesome.zip
unzip fontawesome.zip

# cd into the extracted folder (cannot be sure what the name is,
# because it contains version number)
pushd */

mv ./fonts/fontawesome-webfont.ttf ~/.fonts/

popd
popd

