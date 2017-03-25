#!/usr/bin/env bash

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

installers=$(ls $SCRIPT_DIR | sort | grep "[0-9]\{3\}")

for installer in $installers; do
    $SCRIPT_DIR/$installer
done
