#!/usr/bin/env python3
"""iTerm2 AutoLaunch shim. Hands over to the uv-managed hints daemon.

The exec keeps the iTerm2 API cookie and key from the environment.
"""
import os
import shutil

uv = shutil.which("uv") or "/opt/homebrew/bin/uv"
script = os.path.expanduser("~/.config/iterm-hints/iterm_hints.py")
os.execv(uv, [uv, "run", "--script", script])
