# 10 ms for key sequences
export DOTFILES_DIR=~/.dotfiles
export KEYTIMEOUT=1
export GO_PATH=$HOME/go
export PATH="$HOME/bin:/usr/local/bin:$DOTFILES_DIR/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$GO_PATH/bin:$DOTFILES_DIR/submodules/tfenv/bin:$PATH"
export LANG=en_US.UTF-8

bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history

bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^[[3~' delete-char
bindkey '^w' backward-kill-word

bindkey '^r' history-incremental-search-backward

bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '\e.' insert-last-word

autoload edit-command-line
zle -N edit-command-line

bindkey -a v begin-selection
bindkey -M vicmd v edit-command-line
bindkey '^X^E' edit-command-line

bindkey -a 'u' undo


export EDITOR=nvim

alias vim=nvim

# convert a text in javascript object notation (from stdin) to json notation
alias js2json='node -e '"'"'console.log(JSON.stringify(eval(`x=${require(`fs`).readFileSync(0,`utf-8`)}`),null,2))'"'";

alias magit='emacs --eval "(after! magit (progn (magit-status \"$(pwd)\") (delete-other-windows)))" 2>/dev/null &!'

fpath=($fpath $DOTFILES_DIR/.zfunctions/)

autoload -U promptinit; promptinit
prompt pure

autoload -Uz compinit
compinit

autoload -U zcalc
autoload -Uz transfer

setopt histignorealldups    # ignore duplicates
setopt hist_ignore_space    # do not save commands starting with space to history
setopt share_history
setopt interactivecomments  # enable comments in REPL

export HISTSIZE=10000
export SAVEHIST=10000
export HISTFILE=$HOME/.zsh_history

export NVM_COMPLETION=true
export NVM_LAZY_LOAD=true


zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'

ZGEN_RESET_ON_CHANGE=(${HOME}/.zshrc ${HOME}/.zshrc.local)
source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved ; then
  zgen load buonomo/yarn-completion
  
  zgen load lukechilds/zsh-nvm

  zgen oh-my-zsh
  zgen oh-my-zsh plugins/gitfast
  zgen oh-my-zsh plugins/docker
  zgen oh-my-zsh plugins/docker-compose
  zgen oh-my-zsh plugins/git

  zgen load zsh-users/zsh-autosuggestions
  zgen load jeffreytse/zsh-vi-mode
  zgen load agkozak/zsh-z

  zgen save
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source $HOME/.dotfiles/submodules/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

export SNIPPET_DIR=~/.dotfiles/snippets

for f in $HOME/.dotfiles/zsh-lib/*.sh; do
    source $f
done

[ -f ~/.zshrc.local ] && source ~/.zshrc.local

export YVM_DIR=/Users/romande/.yvm
[ -r $YVM_DIR/yvm.sh ] && . $YVM_DIR/yvm.sh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"


