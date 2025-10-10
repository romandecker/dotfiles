findconfig() {
  # from: https://www.npmjs.com/package/find-config#algorithm
  # 1. If X/file.ext exists and is a regular file, return it. STOP
  # 2. If X has a parent directory, change X to parent. GO TO 1
  # 3. Return NULL.

  if [ -f "$1" ]; then
    printf '%s\n' "${PWD%/}/$1"
  elif [ "$PWD" = / ]; then
    false
  else
    # a subshell so that we don't affect the caller's $PWD
    (cd .. && findconfig "$1")
  fi
}


detect_pm_from_package_json() {
  pkg_json="$1"
  dir=$(dirname "$pkg_json")

  # 1. Check packageManager field
  if command -v jq >/dev/null 2>&1; then
    pm=$(jq -r '.packageManager? // empty' "$pkg_json")
    if [ -n "$pm" ] && [ "$pm" != "null" ]; then
      echo "${pm%%@*}"
      return 0
    fi
  else
    # simple sed fallback (not a full JSON parser but fine for this field)
    pm=$(sed -n 's/.*"packageManager"[[:space:]]*:[[:space:]]*"\([^"@]*\).*/\1/p;q' "$pkg_json")
    if [ -n "$pm" ]; then
      echo "$pm"
      return 0
    fi
  fi

  # 2. Check lockfiles in same directory
  if [ -f "$dir/pnpm-lock.yaml" ]; then
    echo "pnpm"
  elif [ -f "$dir/yarn.lock" ]; then
    echo "yarn"
  elif [ -f "$dir/bun.lockb" ]; then
    echo "bun"
  elif [ -f "$dir/package-lock.json" ] || [ -f "$dir/npm-shrinkwrap.json" ]; then
    echo "npm"
  else
    # 3. fallback
    echo "npm"
  fi
}

pm() {
  local packageJson
  packageJson=$(findconfig "package.json") || return
  [ -f "$packageJson" ] || { echo "package.json not found"; return 1; }

  local pm
  pm=$(detect_pm_from_package_json "$packageJson")

  # choose correct runner per PM
  local runner
  case "$pm" in
    yarn) runner="yarn" ;;
    npm)  runner="npm run" ;;
    pnpm) runner="pnpm run" ;;
    bun)  runner="bun run" ;;
    *)    runner="$pm run" ;;  # conservative default
  esac

  # let the user pick a script (if none, bail)
  local script
  script=$(
    jq -r '.scripts? | keys[]?' "$packageJson" \
    | fzf --header "${runner} <what?>" \
          --preview="jq -r '.scripts[\"{}\"]' \"$packageJson\"" \
          --height=40%
  ) || return



  # prefill the command line in zsh; fall back to echo for bash
  if command -v print >/dev/null 2>&1; then
    print -z -- "$runner $script"
  else
    printf '%s\n' "$runner $script"
  fi
}
