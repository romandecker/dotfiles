#!/bin/bash

# Read JSON input once
input=$(cat)

# Get git info from existing script
git_info=$(echo "$input" | bash ~/.claude/statusline-command.sh)

# Pull context-window stats from the JSON Claude Code provides on stdin.
# total_input_tokens = tokens currently in the context window (input + cache).
# used_percentage    = pre-calculated % of the window used.
ctx=$(echo "$input" | jq -r '"\(.context_window.total_input_tokens // 0)\t\(.context_window.used_percentage // 0)"')
tokens=$(printf '%s' "$ctx" | cut -f1)
pct=$(printf '%s' "$ctx" | cut -f2)

# Abbreviate the token count (46600 -> 46.6k, 8000 -> 8k, 999 -> 999)
tokens_fmt=$(awk -v n="$tokens" 'BEGIN {
  if (n >= 1000) {
    s = sprintf("%.1f", n / 1000)
    sub(/\.0$/, "", s)          # 8.0k -> 8k, but keep 46.6k
    printf "%sk", s
  } else {
    printf "%d", n
  }
}')

# Percentage with one decimal (5 -> 5.0)
pct_fmt=$(awk -v p="$pct" 'BEGIN { printf "%.1f", p }')

# Combine: git info | 46.6k (5.0%)
printf '%s | \033[01;33m%s (%s%%)\033[00m' "$git_info" "$tokens_fmt" "$pct_fmt"
