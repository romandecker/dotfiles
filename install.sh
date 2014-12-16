#!/bin/bash

echo "This script requires the node executable to be available in your \$PATH!"

pushd installer
npm install && node install.js
popd
