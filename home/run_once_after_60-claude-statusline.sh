#!/usr/bin/env bash
# Claude Code statusline (was installer/024-claude.sh):
#  - ccstatusline npm global (best-effort; needs node)
#  - merge the statusLine key into ~/.claude/settings.json WITHOUT clobbering it
# The statusline scripts themselves + ~/.config/ccstatusline/settings.json are
# managed as regular dotfiles.
set -eu

if command -v npm >/dev/null 2>&1; then
    npm install -g ccstatusline || true
fi

CLAUDE_SETTINGS="$HOME/.claude/settings.json"
if command -v jq >/dev/null 2>&1; then
    mkdir -p "$(dirname "$CLAUDE_SETTINGS")"
    [ -f "$CLAUDE_SETTINGS" ] || echo '{}' > "$CLAUDE_SETTINGS"
    tmp="$(mktemp)"
    jq '.statusLine = {"type": "command", "command": "bash ~/.claude/statusline-wrapper.sh"}' \
        "$CLAUDE_SETTINGS" > "$tmp" && mv "$tmp" "$CLAUDE_SETTINGS"
fi
