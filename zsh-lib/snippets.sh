#!/usr/bin/env bash

function snippet {
    local cmd=$(pet search)
    print -z "$cmd"
}
