#!/usr/bin/env bash

function snippet {
    local s=$(find $SNIPPET_DIR -type f -printf "%f\n" | fzf --preview "{ cat $SNIPPET_DIR/{} }" --height 40% --reverse)
    cat $SNIPPET_DIR/$s
}
