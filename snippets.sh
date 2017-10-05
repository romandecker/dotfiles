#!/usr/bin/env bash

function snippet {
    local s=$((cd $SNIPPET_DIR; find . -type f) | sed 's#\./##' | fzf --preview "{ highlight-snippet $SNIPPET_DIR/{} }" --height 40% --reverse)

    if [ ! -z "$s" ]; then
      highlight-snippet $SNIPPET_DIR/$s
    fi
}

function create-snippet {

    if [ -z "$1" ]; then
      echo "Usage: create-snippet <name> [snippet]"
      return 1
    fi

    if [ ! -z "$2" ]; then
      echo "Creating snippet $SNIPPET_DIR/$1"
      echo "${@:2}" > $SNIPPET_DIR/$1
    fi

    $EDITOR $SNIPPET_DIR/$1
}
