alias 'edit-project'='tmuxifier es'
alias 'list-projects'='tmuxifier ls'
alias 'grep-projects'='tmuxifier ls | grep -i'
alias gen='HYGEN_TMPLS=~/.hygen-templates npx hygen'

project() {
    if [ -z "$1" ]; then
        print -z "project $(tmuxifier ls | fzf)"
    else
        tmuxifier s $1
    fi
}

new-project() {
    local project_name=$1

    pushd $PROJECTS_PATH

    if [[ $project_name =~ [:/] ]];
    then
        # if a second argument is given, use that as project name, else, derive
        # it from first argument
        project_name="${2:-$(echo $project_name | sed 's#.*/\(.*\)\.git$#\1#')}"
        git clone $1 $project_name
    else
        mkdir -p $PROJECTS_PATH/$project_name
        pushd $PROJECTS_PATH/$project_name
        git init
        git commit --allow-empty -m "Initial commit"
        popd
    fi

    popd
    tmuxifier ns "$project_name"
    project "$project_name"
}

delete-project() {
    if [ -z "$1" ]; then
        echo "Must provide a project to delete!"
    else
        rm -rf $PROJECTS_PATH/$1
        rm -f $TMUXIFIER_LAYOUT_PATH/$1.session.sh
    fi
}

export PROJECTS_PATH=$HOME/projects
export TMUXIFIER_LAYOUT_PATH="$HOME/.tmux-layouts"
# tmuxifier code is installed via .chezmoiexternal.toml -> ~/.local/share/tmuxifier
export PATH=$PATH:$HOME/.local/share/tmuxifier/bin
if command -v tmuxifier >/dev/null 2>&1; then
    eval "$(tmuxifier init -)"
fi
export TMUXIFIER_TMUX_OPTS=-2
