#!/bin/sh
# SessionStart hook for the task-observer skill (installed by
# run_onchange_after_48-task-observer.sh).
# Injects the activation instruction plus the observation log state, so the
# skill loads every session instead of relying on description matching.
# Based on references/environments.md in the skill bundle.
d="$HOME/.claude/skill-observations"

# Count files whose status is open, not all files: resolved entries stay a day.
open=$(find "$d/observation-log" -maxdepth 1 -name '*.md' -exec grep -l '^status: open$' {} + 2>/dev/null | wc -l | tr -d ' ')
last=$(cat "$d/last-review-date.txt" 2>/dev/null || echo never)

msg="Before the first tool call, and before writing or proposing a plan, invoke the task-observer skill AND run its Session Start Protocol. Loading the skill and running the protocol are separate steps. Workspace folder: $d (user scope, shared across projects). After each task, report a one-line summary of observations logged this session."
if [ "$open" -gt 0 ]; then
  msg="$msg $open open observations; last review: $last."
  cutoff=$(date -v-7d +%Y-%m-%d 2>/dev/null || date -d '7 days ago' +%Y-%m-%d)
  # ISO dates sort lexically: true when $last is not later than $cutoff.
  if [ "$last" = never ] || [ "$(printf '%s\n%s\n' "$last" "$cutoff" | sort | head -1)" = "$last" ]; then
    msg="$msg Offer the review."
  fi
fi

jq -n --arg msg "$msg" '{hookSpecificOutput: {hookEventName: "SessionStart", additionalContext: $msg}}'
