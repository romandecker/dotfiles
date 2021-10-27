alias 'new-project'='tmuxifier ns'
alias 'edit-project'='tmuxifier es'
alias 'list-projects'='tmuxifier ls'
alias 'grep-projects'='tmuxifier ls | grep -i'
alias gen='HYGEN_TMPLS=~/.dotfiles/hygen-templates npx hygen'

project() {
    if [ -z "$1" ]; then
        print -z "project $(tmuxifier ls | fzf)"
    else
        tmuxifier s $1
    fi
}

export TMUXIFIER_LAYOUT_PATH="$DOTFILES_DIR/.tmux-layouts"
export PATH=$PATH:$DOTFILES_DIR/submodules/tmuxifier/bin
eval "$(tmuxifier init -)"
export TMUXIFIER_TMUX_OPTS=-2
