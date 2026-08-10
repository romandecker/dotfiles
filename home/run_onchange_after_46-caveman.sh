#!/usr/bin/env bash
# Caveman skill for Claude Code (https://github.com/JuliusBrussee/caveman):
# compresses agent prose output. No Homebrew formula; installed via its own
# npx-based installer, so it needs `claude` and Node >=18 already present.
# --non-interactive also auto-activates when stdin isn't a TTY (true here),
# but is passed explicitly for clarity. Installer is safe to re-run.
set -eu

if command -v claude >/dev/null 2>&1 && command -v node >/dev/null 2>&1; then
    npx -y github:JuliusBrussee/caveman -- --only claude --non-interactive
fi
