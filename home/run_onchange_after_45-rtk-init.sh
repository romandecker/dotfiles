#!/usr/bin/env bash
# One-time rtk setup (https://github.com/rtk-ai/rtk). rtk patches Claude Code's
# config, so it needs `claude` to already be installed (cask in the Brewfile,
# installed by the run_onchange_before_10 script that runs ahead of this one).
# --auto-patch skips the interactive telemetry prompt so this can run
# unattended; idempotent, so re-running on script changes is safe.
set -eu

if command -v rtk >/dev/null 2>&1 && command -v claude >/dev/null 2>&1; then
    rtk init -g --auto-patch
fi
