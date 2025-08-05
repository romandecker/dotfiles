zle_format_command() {
  emulate -L zsh
  setopt localoptions shwordsplit

  # split buffer into tokens (respecting quotes)
  local -a tokens
  tokens=("${(z)LBUFFER}")

  # pull off the command name
  local cmd=${tokens[1]}
  shift tokens

  local -a out
  out+=("$cmd \\")     # start with "cmd \"

  local i=1
  while (( i <= $#tokens )); do
    local tok=${tokens[i]}

    if [[ $tok == --* ]]; then
      # long option
      if [[ $tok == *=* ]]; then
        out+=("  $tok \\")
      else
        if (( i+1 <= $#tokens )) && [[ ${tokens[i+1]} != -* ]]; then
          out+=("  $tok ${tokens[i+1]} \\")
          (( i+=2 )); continue
        else
          out+=("  $tok \\")
        fi
      fi

    elif [[ $tok =~ '^-[[:alnum:]]$' ]]; then
      # if next token is a non-flag, treat this as opt+arg
      if (( i+1 <= $#tokens )) && [[ ${tokens[i+1]} != -* ]]; then
        out+=("  $tok ${tokens[i+1]} \\")
        (( i+=2 )); continue
      fi

      # otherwise group successive lone flags
      local letters=${tok#-}
      (( i++ ))
      while (( i <= $#tokens )) && [[ ${tokens[i]} =~ '^-[[:alnum:]]$' ]] \
            && { (( i+1 > $#tokens )) || [[ ${tokens[i+1]} == -* ]]; }; do
        letters+="${tokens[i]#-}"
        (( i++ ))
      done
      out+=("  -$letters \\")
      continue

    else
      # bare argument
      out+=("  $tok \\")
    fi

    (( i++ ))
  done

  # join with real newlines, strip trailing " \" from last line
  local formatted
  formatted=$(printf "%s\n" "${out[@]}")
  formatted=${formatted/% \\$/}

  LBUFFER=$formatted
  zle redisplay
}

zle -N zle_format_command