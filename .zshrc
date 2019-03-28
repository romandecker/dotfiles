# 10 ms for key sequences
export DOTFILES_DIR=~/.dotfiles
export KEYTIMEOUT=1
export GO_PATH=$HOME/go
export PATH="$HOME/bin:/usr/local/bin:$DOTFILES_DIR/bin:$HOME/.fzf/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$GO_PATH/bin:$PATH"
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

fpath=($fpath $DOTFILES_DIR/.zfunctions/)

autoload -U promptinit; promptinit
prompt pure

autoload -Uz compinit
compinit

autoload -U zcalc
autoload -Uz transfer

setopt histignorealldups    # ignore duplicates
setopt hist_ignore_space    # do not save commands starting with space to history
setopt inc_append_history
setopt share_history

export HISTSIZE=1000
export SAVEHIST=1000
export HISTFILE=$HOME/.zsh_history


zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'

source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved ; then
  zgen load wbingli/zsh-wakatime

  zgen oh-my-zsh
  zgen oh-my-zsh/plugins/gitfast

  zgen save
fi


source $HOME/.dotfiles/submodules/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOME/.dotfiles/submodules/zsh-autosuggestions/zsh-autosuggestions.zsh
source $HOME/.dotfiles/z/z.sh

export SNIPPET_DIR=~/.dotfiles/snippets

for f in $HOME/.dotfiles/zsh-lib/*.sh; do
    source $f
done

[ -f ~/.zshrc.local ] && source ~/.zshrc.local
