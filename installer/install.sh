#!/usr/bin/env bash

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $SCRIPT_DIR/utils.sh

installers=$(ls $SCRIPT_DIR | sort | grep "[0-9]\{3\}")

for installer in $installers; do
    name=$(echo $installer | sed 's/^[0-9]*-//')
    name="${name%.*}"

    if [[ $* == *--skip-$name* ]]; then
        section "$name [skipped]"
    else
      section $name
      $SCRIPT_DIR/$installer
    fi
done


installers=$(ls $SCRIPT_DIR/$os | sort | grep "[0-9]\{3\}")

for installer in $installers; do
    name=$(echo $installer | sed 's/^[0-9]*-//')
    name="${name%.*}"

    if [[ $* == *--skip-$name* ]]; then
        section "$name [skipped]"
    else
      section $name
      $SCRIPT_DIR/$os/$installer
    fi
done
