# fzf-rebase: pick a commit via fzf, load `git rebase -i <sha>` into the
# prompt (does not run it, same idea as snippet.sh's print -z). The listed
# sha is the PARENT of the picked commit, since `rebase -i` takes the commit
# before the range you want to edit; a root commit has no parent, so that
# case falls back to --root.

fzf-rebase() {
    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "fzf-rebase: not a git repository" >&2
        return 1
    fi

    local range="HEAD"
    local upstream_base
    if upstream_base=$(git merge-base '@{upstream}' HEAD 2>/dev/null); then
        range="${upstream_base}..HEAD"
    fi

    local selected
    selected=$(git log "$range" --format='%h%x09%ad%x09%s' --date='format:%Y-%m-%d %H:%M' \
        | fzf --delimiter=$'\t' \
              --preview='git show --stat --color=always {1}' \
              --preview-window=right:60%)

    [ -z "$selected" ] && return

    local sha target
    sha=$(echo "$selected" | cut -f1)
    if git rev-parse --verify -q "${sha}^" >/dev/null 2>&1; then
        target="${sha}^"
    else
        target="--root"
    fi

    print -z "git rebase -i $target"
}
