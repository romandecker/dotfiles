#!/usr/bin/env bash
# Stage a copy of the repo and run the full Linux chezmoi bootstrap in Docker.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "==> Staging repo into test/workspace"
rm -rf "$SCRIPT_DIR/workspace"
mkdir -p "$SCRIPT_DIR/workspace"
# Copy the current working tree (includes uncommitted work), minus git + itself.
tar -c --exclude='./.git' --exclude='./test/workspace' -C "$REPO_ROOT" . \
    | tar -x -C "$SCRIPT_DIR/workspace"

echo "==> Building image (runs the full bootstrap)"
docker build -t dotfiles-test "$SCRIPT_DIR"

echo "==> Verifying key symlinks / files in the container"
docker run --rm dotfiles-test bash -lc '
  set -e
  for f in ~/.zshrc ~/.vimrc ~/.tmux.conf ~/.gitignore ~/.local/bin/git-browse-pr; do
    if [ -e "$f" ]; then echo "OK   $f -> $(readlink -f "$f")"; else echo "MISS $f"; exit 1; fi
  done
  echo "All checks passed."
'
