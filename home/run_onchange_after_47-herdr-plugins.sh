#!/usr/bin/env bash
# herdr-last-workspace plugin (https://github.com/akpw/herdr-last-workspace):
# tmux-style MRU toggle between the two most recently focused workspaces.
# Pinned to a commit since the repo has no tags. Install is safe to re-run.
set -eu

if command -v herdr >/dev/null 2>&1; then
    herdr plugin install akpw/herdr-last-workspace --ref 33d88d69f7724af8c34b38013e74fbc95f99fd72 -y
fi
