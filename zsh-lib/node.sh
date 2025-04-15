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


pm() {
    local packageJson=$(findconfig "package.json")
    local script=$(jq -r '.scripts | keys []' $packageJson \
                       | fzf --header "$1 <what?>" \
                             --preview="jq '.scripts[\"{}\"]' $packageJson" --height="40%")
    print -z $1 $script
}

np() {
  pm "npm run"
}

pnp() {
  pm "pnpm run"
}

y() {
  pm "yarn"
}
