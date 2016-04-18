bindkey '^R' history-incremental-search-backward
bindkey '\e.' insert-last-word
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line

bindkey '^[b' backward-word # mac
bindkey '^[f' forward-word  # mac

bindkey -a v begin-selection
bindkey -M vicmd v edit-command-line

if [ `uname` = "Darwin" ]; then
	bindkey    "^[[3~"          delete-char
	bindkey    "^[3;5~"         delete-char
fi
