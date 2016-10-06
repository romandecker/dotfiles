if [ "$EMACS" = "" ]
then

  setopt prompt_subst
  autoload -U colors && colors # Enable colors in prompt


  ANUM="ANUM"
  BNUM="BNUM"
  BLUE="%{$fg[blue]%}"
  GREEN="%{$fg[green]%}"
  RED="%{$fg[red]%}"
  CYAN="%{$fg[cyan]%}"
  YELLOW="%{$fg[yellow]%}"
  RESET="%{$reset_color%}"



  # Modify the colors and symbols in these variables as desired.
  GIT_PROMPT_SYMBOL="$BLUE±$RESET"
  GIT_PROMPT_PREFIX="$GREEN$RESET"
  GIT_PROMPT_SUFFIX="$GREEN$RESET"
  GIT_PROMPT_AHEAD="$RED$ANUM$RESET"
  GIT_PROMPT_BEHIND="$CYAN$BNUM$RESET"
  GIT_PROMPT_MERGING="$RED⤚$RESET"
  GIT_PROMPT_UNTRACKED="$RED●$RESET"
  GIT_PROMPT_MODIFIED="$YELLOW●$RESET"
  GIT_PROMPT_STAGED="$GREEN●$RESET"

  # Show Git branch/tag, or name-rev if on detached head
  parse_git_branch() {
    (git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD) 2> /dev/null
  }

  # # Show different symbols as appropriate for various Git repository states
  parse_git_state() {

    # Compose this value via multiple conditional appends.
    local GIT_STATE=""

    local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
    if [ "$NUM_AHEAD" -gt 0 ]; then
      GIT_STATE=$GIT_STATE${GIT_PROMPT_AHEAD//NUM/$NUM_AHEAD}
    fi

    local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
    if [ "$NUM_BEHIND" -gt 0 ]; then
      GIT_STATE=$GIT_STATE${GIT_PROMPT_BEHIND//NUM/$NUM_BEHIND}
    fi

    local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
    if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
      GIT_STATE=$GIT_STATE$GIT_PROMPT_MERGING
    fi

    if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
      GIT_STATE=$GIT_STATE$GIT_PROMPT_UNTRACKED
    fi

    if ! git diff --quiet 2> /dev/null; then
      GIT_STATE=$GIT_STATE$GIT_PROMPT_MODIFIED
    fi

    if ! git diff --cached --quiet 2> /dev/null; then
      GIT_STATE=$GIT_STATE$GIT_PROMPT_STAGED
    fi

    if [[ -n $GIT_STATE ]]; then
      echo "$GIT_PROMPT_PREFIX$GIT_STATE$GIT_PROMPT_SUFFIX"
    fi

  }

  # # If inside a Git repository, print its branch and state
  git_prompt_string() {
    local git_where="$(parse_git_branch)"
    [ -n "$git_where" ] && echo "$GIT_PROMPT_SYMBOL$(parse_git_state)$GIT_PROMPT_PREFIX$YELLOW${git_where#(refs/heads/|tags/)}$GIT_PROMPT_SUFFIX"
  }


  # %B -> bold
  # %n -> $USERNAME
  # %m -> hostname up to first '.'
  # %M -> %~ path, with home shortened to ~, use a count to only show so many
  # %# -> '#' if root '!' otherwise
  # %(x.true-text.false-text)
  ROOT_INDICATOR="%(!.$RED%n$RESET@.)"
  PROMPT_ARROW="$YELLOW%(!.#.λ)$RESET"

  PROMPT="%B$ROOT_INDICATOR%3~$PROMPT_ARROW%b "

  # Set the right-hand prompt
  RPS1='$(git_prompt_string)'

else
  PROMPT="%B%3~$ %b"
fi
