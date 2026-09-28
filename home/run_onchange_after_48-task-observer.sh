#!/usr/bin/env bash
# task-observer skill, aka "One Skill to Rule Them All"
# (https://github.com/rebelytics/one-skill-to-rule-them-all): logs
# improvement observations across sessions. Installed globally via the
# skills.sh CLI, so it needs `claude` and Node >=18 already present.
# -y skips confirmation prompts; install is safe to re-run.
set -eu

if command -v claude >/dev/null 2>&1 && command -v node >/dev/null 2>&1; then
    npx -y skills add rebelytics/one-skill-to-rule-them-all --skill task-observer -g -y
fi
