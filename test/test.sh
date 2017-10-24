#!/usr/bin/env bash

set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_DIR=$SCRIPT_DIR/..
WORKSPACE_DIR=$SCRIPT_DIR/workspace
TESTING_BRANCH=test-installer

git stash --include-untracked > /dev/null

git rev-parse --verify $TESTING_BRANCH && git branch -D $TESTING_BRANCH > /dev/null

echo "Ensuring branch $TESTING_BRANCH to hold the current repo state"
git checkout -b $TESTING_BRANCH

git stash apply > /dev/null

git add $REPO_DIR > /dev/null
git commit -m "Commit for test" > /dev/null
git checkout -
git stash pop > /dev/null


rm -rf $WORKSPACE_DIR
git clone -b $TESTING_BRANCH $REPO_DIR $WORKSPACE_DIR

docker build $SCRIPT_DIR -t dotfiles-test
docker run -it dotfiles-test /home/alice/.dotfiles/test/run-test.sh
# docker rmi -f dotfiles-test
