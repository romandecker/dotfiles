# 10 ms for key sequences
export KEYTIMEOUT=1 
export PATH=$HOME/bin:/usr/local/bin:$PATH
export LANG=en_US.UTF-8

export DOTFILES_DIR=~/.dotfiles

bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history

bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^[[3~' delete-char
bindkey '^w' vi-backward-kill-word

bindkey '^r' history-incremental-search-backward

bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '\e.' insert-last-word

autoload edit-command-line
zle -N edit-command-line

bindkey -a v begin-selection
bindkey -M vicmd v edit-command-line
bindkey '^X^E' edit-command-line

alias vim=nvim
alias 'new-project'='tmuxifier ns'
alias 'edit-project'='tmuxifier es'
alias 'list-projects'='tmuxifier ls'
alias 'grep-projects'='tmuxifier ls | grep -i'
alias project='tmuxifier s'

fpath=($fpath $DOTFILES_DIR/.zfunctions/)

autoload -U promptinit; promptinit
prompt pure

autoload -Uz compinit
compinit

autoload -U zcalc

setopt histignorealldups    # ignore duplicates
setopt hist_ignore_space    # do not save commands starting with space to history
setopt inc_append_history
setopt share_history

export HISTSIZE=1000
export SAVEHIST=1000
export HISTFILE=$HOME/.zsh_history


export TMUXIFIER_LAYOUT_PATH="$DOTFILES_DIR/.tmux-layouts"
export PATH=$PATH:$DOTFILES_DIR/submodules/tmuxifier/bin
eval "$(tmuxifier init -)"
export TMUXIFIER_TMUX_OPTS=-2

killport() {
  pid=$(lsof -i :$1 | tail -n+2 | head -1 | awk '{ print $2 }')

  kill -9 $pid
}

encrypt-for() {
  b64=$(gpg -e --recipient $1 | base64)

  echo "Send this to $1:"
  echo "echo $b64 | base64 -D | gpg -d"
}

docker-cleanup() {
  docker rmi $(docker images -q -f dangling=true)
}


source ~/.dotfiles/submodules/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.dotfiles/submodules/zsh-autosuggestions/zsh-autosuggestions.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

source ~/.dotfiles/snippets.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.zshrc.local ] && source ~/.zshrc.local

