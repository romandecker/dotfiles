#!/usr/bin/env bash
# Install neovim plugins once (was installer/006-vim.sh). Best-effort: skip if
# neovim isn't on PATH yet (e.g. Brewfile step was skipped).
set -eu

# neovim python provider (was `pip install neovim` in installer/006-vim.sh)
if command -v pip3 >/dev/null 2>&1; then
    pip3 install --user --quiet pynvim || true
fi

if command -v nvim >/dev/null 2>&1; then
    nvim --headless -c 'PlugInstall --sync' -c 'qall' || true
fi
