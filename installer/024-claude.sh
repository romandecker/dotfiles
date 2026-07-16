#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

# Install ccstatusline (provides the context-window percentage segment)
if [ -x "$(command -v ccstatusline)" ]; then
    cecho "$fawn"ccstatusline"$normal is already installed"
elif [ -x "$(command -v npm)" ]; then
    echo "Installing ccstatusline globally via npm..."
    npm install -g ccstatusline
else
    echo "$fawn"npm"$normal not found on PATH, skipping ccstatusline install."
    echo "The statusline will still work but without the context percentage."
    echo "Install node/npm and re-run, or run: npm install -g ccstatusline"
fi

# Link the statusline scripts and ccstatusline config
link_dotfile claude/statusline-command.sh .claude/statusline-command.sh
link_dotfile claude/statusline-wrapper.sh .claude/statusline-wrapper.sh
link_dotfile claude/ccstatusline-settings.json .config/ccstatusline/settings.json

# Merge the statusLine key into ~/.claude/settings.json (a live file Claude Code
# writes to itself, so we never symlink it -- just patch the one key in place).
CLAUDE_SETTINGS="$HOME/.claude/settings.json"

mkdir -p "$(dirname "$CLAUDE_SETTINGS")"
if [ ! -f "$CLAUDE_SETTINGS" ]; then
    echo "{}" > "$CLAUDE_SETTINGS"
fi

tmp=$(mktemp)
jq '.statusLine = {"type": "command", "command": "bash ~/.claude/statusline-wrapper.sh"}' \
    "$CLAUDE_SETTINGS" > "$tmp" && mv "$tmp" "$CLAUDE_SETTINGS"

cecho "Merged $fawn statusLine $normal into $fawn$CLAUDE_SETTINGS$normal"
