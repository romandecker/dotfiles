#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

link_dotfile dotgitignore .gitignore

echo "Configuring global git excludesfile"
git config --global core.excludesfile $HOME/.gitignore


git config --global alias.co '!git checkout $(git branch | fzf)'

# allow doing `git push -u` without explicitly having to type branch-name
git config --global push.default current

# Always show 3-way diff
git config --global merge.conflictstyle diff3

(ensure git-delta delta \
    && git config --global alias.side-by-side-diff '!git -c delta.side-by-side=true diff' \
    && git config --global core.pager delta \
    && git config --global delta.features "line-numbers decorations" \
    && git config --global delta.line-numbers true \
    ) || true

ensure git-extras git-summary
