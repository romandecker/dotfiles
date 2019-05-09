#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

VSCODE_USER_CONFIG_DIR="Library/Application Support/Code/User"

if [ $os == $LINUX ]; then
    VSCODE_USER_CONFIG_DIR=".config/Code/User"
else
    defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false
fi

link_dotfile vscode "$VSCODE_USER_CONFIG_DIR"

while [ ! -x "$(command -v code)" ]; do
    echo "Visual Studio Code is not installed"
    echo "Please install, run and run \"Shell Command: Install 'code' command in PATH\" from the command palette"
    echo "Press any key when installed"
    read
done

code \
    --force \
    --install-extension CoenraadS.bracket-pair-colorizer \
    --install-extension SirTori.indenticator \
    --install-extension WallabyJs.quokka-vscode \
    --install-extension christian-kohler.npm-intellisense \
    --install-extension christian-kohler.path-intellisense \
    --install-extension dbaeumer.vscode-eslint \
    --install-extension eamodio.gitlens \
    --install-extension esbenp.prettier-vscode \
    --install-extension flowtype.flow-for-vscode \
    --install-extension jpoissonnier.vscode-styled-components \
    --install-extension kumar-harsh.graphql-for-vscode \
    --install-extension msjsdiag.debugger-for-chrome \
    --install-extension naumovs.color-highlight \
    --install-extension streetsidesoftware.code-spell-checker \
    --install-extension vscodevim.vim \
    --install-extension wayou.vscode-todo-highlight \
    --install-extension wix.vscode-import-cost \
    --log info
