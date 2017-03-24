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
bindkey '^w' backward-kill-word

bindkey '^r' history-incremental-search-backward

bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '\e.' insert-last-word

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

setopt histignorealldups
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=$HOME/zsh_history

export PATH=$PATH:$DOTFILES_DIR/submodules/tmuxifier/bin
eval "$(tmuxifier init -)"
export TMUXIFIER_TMUX_OPTS=-2

killport() {
  pid=$(lsof -i :$1 | tail -n+2 | head -1 | awk '{ print $2 }')

  kill -9 $pid
}

if [ -f $HOME/zshrc.local ]; then
    # if a local config file exists, source it
    source $HOME/zshrc.local
fi

