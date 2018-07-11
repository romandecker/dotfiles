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

echo "Setting zsh as your default shell"
shell=$(which zsh)
if [ -z "$(grep $shell /etc/shells)" ]; then
    echo "$(which zsh) is not part of /etc/shells, appending..."
    echo $(which zsh) | sudo tee -a /etc/shells
fi

chsh -s $(which zsh)

clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
