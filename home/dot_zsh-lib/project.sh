# project/new-project/delete-project/list-projects/grep-projects/edit-project
# work against either tmux (via tmuxifier) or herdr, selected with --type or
# defaulted from PROJECT_TYPE. Project roots are read from/written to the
# existing tmuxifier .session.sh files (session_root "..."), so both backends
# share one source of truth for where a project lives.

export PROJECT_TYPE="${PROJECT_TYPE:-herdr}"

_project_parse_type() {
    _PROJECT_TYPE="$PROJECT_TYPE"
    _PROJECT_ARGS=()
    while [ $# -gt 0 ]; do
        case "$1" in
            --type)
                _PROJECT_TYPE="$2"
                shift 2
                ;;
            --type=*)
                _PROJECT_TYPE="${1#--type=}"
                shift
                ;;
            *)
                _PROJECT_ARGS+=("$1")
                shift
                ;;
        esac
    done
}

_project_root() {
    local name=$1
    local session_file="$TMUXIFIER_LAYOUT_PATH/$name.session.sh"
    if [ -f "$session_file" ]; then
        local root
        root=$(grep -m1 '^session_root ' "$session_file" | sed -E 's/^session_root "(.*)"$/\1/')
        if [ -n "$root" ]; then
            eval echo "$root"
            return
        fi
    fi
    echo "$PROJECTS_PATH/$name"
}

_herdr_workspace_id() {
    local name=$1
    herdr workspace list 2>/dev/null | jq -r --arg l "$name" '.result.workspaces[] | select(.label==$l) | .workspace_id' | head -n1
}

_herdr_switch_or_create() {
    local name=$1
    local root=$2
    local id
    id=$(_herdr_workspace_id "$name")
    if [ -n "$id" ]; then
        herdr workspace focus "$id" >/dev/null
    else
        herdr workspace create --cwd "$root" --label "$name" --focus >/dev/null
    fi
}

project() {
    _project_parse_type "$@"
    set -- "${_PROJECT_ARGS[@]}"
    local type="$_PROJECT_TYPE"

    if [ -z "$1" ]; then
        local picked
        picked=$(list-projects | fzf)
        [ -n "$picked" ] && print -z "project --type $type $picked"
        return
    fi

    local name=$1

    if [ "$type" = "tmux" ]; then
        tmuxifier s "$name"
        return
    fi

    if [ "$type" != "herdr" ]; then
        echo "Unknown project type: $type (expected tmux or herdr)" >&2
        return 1
    fi

    local root
    root=$(_project_root "$name")

    if [ -n "$HERDR_ENV" ]; then
        _herdr_switch_or_create "$name" "$root"
    else
        if [ "$(herdr status server --json 2>/dev/null | jq -r .running)" = "true" ]; then
            _herdr_switch_or_create "$name" "$root"
        fi
        exec herdr
    fi
}

new-project() {
    _project_parse_type "$@"
    set -- "${_PROJECT_ARGS[@]}"
    local type="$_PROJECT_TYPE"

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
    project --type "$type" "$project_name"
}

delete-project() {
    _project_parse_type "$@"
    set -- "${_PROJECT_ARGS[@]}"

    if [ -z "$1" ]; then
        echo "Must provide a project to delete!"
        return 1
    fi

    local name=$1

    tmux kill-session -t "$name" >/dev/null 2>&1

    local id
    id=$(_herdr_workspace_id "$name")
    [ -n "$id" ] && herdr workspace close "$id" >/dev/null 2>&1

    rm -rf $PROJECTS_PATH/$name
    rm -f $TMUXIFIER_LAYOUT_PATH/$name.session.sh
}

edit-project() {
    _project_parse_type "$@"
    set -- "${_PROJECT_ARGS[@]}"

    if [ -z "$1" ]; then
        echo "Must provide a project to edit!"
        return 1
    fi

    ${=EDITOR:-vi} "$TMUXIFIER_LAYOUT_PATH/$1.session.sh"
}

alias 'list-projects'='tmuxifier ls'
alias 'grep-projects'='tmuxifier ls | grep -i'
