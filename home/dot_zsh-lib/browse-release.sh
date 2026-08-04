# browse-release: open the routeTable host of the helm release matching the
# current git branch. Filters `helm ls` on a cleaned/anchored branch name,
# then pulls routeTable.hosts out of the release's deployed values via jq
# (nesting varies per chart, so the search is recursive rather than a fixed
# path). Falls back to fzf whenever a step has more than one candidate.

_browse_release_clean_branch() {
    git branch --show-current 2>/dev/null \
        | tr '[:upper:]' '[:lower:]' \
        | sed -E 's/[^a-z0-9]+/-/g; s/^-+//; s/-+$//'
}

browse-release() {
    local releases release

    if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        local branch
        branch=$(_browse_release_clean_branch)
        if [ -z "$branch" ]; then
            echo "browse-release: not on a git branch" >&2
            return 1
        fi

        local pattern="(^|-)${branch}(-|\$)"
        releases=$(helm ls -f "$pattern")
        if [ -z "$(echo "$releases" | tail -n +2)" ]; then
            echo "browse-release: no helm release matching branch '$branch' in current namespace" >&2
            return 1
        fi

        if [ "$(echo "$releases" | tail -n +2 | wc -l | tr -d ' ')" = "1" ]; then
            release=$(echo "$releases" | tail -n +2 | awk '{print $1}')
        else
            release=$(echo "$releases" | fzf --header-lines=1 | awk '{print $1}')
            [ -z "$release" ] && return 1
        fi
    else
        releases=$(helm ls)
        if [ -z "$(echo "$releases" | tail -n +2)" ]; then
            echo "browse-release: no helm releases found in current namespace" >&2
            return 1
        fi

        release=$(echo "$releases" | fzf --header-lines=1 | awk '{print $1}')
        [ -z "$release" ] && return 1
    fi

    local hosts
    hosts=$(helm get values "$release" -o json \
        | jq -r '[.. | objects | .routeTable? | .hosts? // empty | .[]?] | .[]')
    if [ -z "$hosts" ]; then
        echo "browse-release: no routeTable.hosts found for release '$release'" >&2
        return 1
    fi

    local host
    if [ "$(echo "$hosts" | wc -l | tr -d ' ')" = "1" ]; then
        host="$hosts"
    else
        host=$(echo "$hosts" | fzf)
        [ -z "$host" ] && return 1
    fi

    open "https://${host}"
}
