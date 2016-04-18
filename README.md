.dotfiles
=========

This repository contains dotfiles that set up a development environment that
uses the following tools:

* zsh
 * zsh-git-prompt
* vim
 * Full .vimrc and Plugins
* tmux
 * tmuxifier

To install or update the dotfiles, clone this repository and run `./install.sh`
from inside it. Make sure that the repo stays where it was when you executed the
installer or else, the symlinks will break. In that case you can always run
`./install.sh` again :).

## Server installation

```sh
curl | https://raw.githubusercontent.com/DeX3/dotfiles/master/server-install.sh
```


