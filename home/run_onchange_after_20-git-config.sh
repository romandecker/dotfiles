#!/usr/bin/env bash
# Global git config (was installer/004-git.sh). Idempotent; re-runs when changed.
set -eu

git config --global core.excludesfile "$HOME/.gitignore"
git config --global alias.co '!git checkout $(git branch | fzf)'
git config --global push.default current
git config --global merge.conflictstyle diff3
git config --global branch.autoSetupMerge always
git config --global push.autoSetupRemote true

# delta-powered diffs (delta comes from the Brewfile)
if command -v delta >/dev/null 2>&1; then
    git config --global alias.side-by-side-diff '!git -c delta.side-by-side=true diff'
    git config --global core.pager delta
    git config --global delta.features "line-numbers decorations"
    git config --global delta.line-numbers true
    git config --global delta.file-decoration-style none
    git config --global delta.hunk-header-decoration-style none
fi
