#!/usr/bin/env bash

function snippet {
    local s=$((cd $SNIPPET_DIR; find . -type f) | sed 's#\./##' | fzf --preview "{ highlight-snippet $SNIPPET_DIR/{} }" --height 40% --reverse)

    if [ ! -z "$s" ]; then
        if [ "$1" = "-e" ] || [ "$1" = "--eval" ]; then
            echo "Running snippet:"
            highlight-snippet $SNIPPET_DIR/$s
            $SHELL $SNIPPET_DIR/$s

        elif [ "$1" = "-p" ] || [ "$1" = "--print" ]; then
            if [ -t 1 ]; then
                highlight-snippet $SNIPPET_DIR/$s
            else
                cat $SNIPPET_DIR/$s
            fi

        else
            local snippet=$(grep -v "^#" $SNIPPET_DIR/$s | sed "s/\\$/\\\\/g")
            print -z "$snippet"
        fi
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
