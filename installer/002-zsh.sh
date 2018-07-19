#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

if [ "$os" == $MAC ]; then
    ensure zsh /usr/local/bin/zsh
else
    ensure zsh
fi


link_dotfile .zshrc

shell=$(which zsh)

if [ "$SHELL" != $shell ]; then
    echo "Setting zsh as your default shell"

    if [ -z "$(grep $shell /etc/shells)" ]; then
        echo "$(which zsh) is not part of /etc/shells, appending..."
        echo $(which zsh) | sudo tee -a /etc/shells
    fi

    chsh -s $(which zsh)
else
    echo "$fawn$SHELL$normal is already the default."
fi

clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
