# /// script
# requires-python = ">=3.11"
# dependencies = ["iterm2"]
# ///
"""Vimium-style link hints for iTerm2.

Runs as an iTerm2 AutoLaunch script. Press the activation shortcut to label every
URL, JIRA ID and file path in the visible viewport. Type a label to open the target.
Esc cancels, Backspace undoes one key.
"""
from __future__ import annotations

import asyncio
import json
import shlex
import shutil
import subprocess
import sys
from pathlib import Path

import logging

import iterm2

logging.basicConfig(
    filename=str(Path("~/.cache/iterm-hints.log").expanduser()),
    level=logging.DEBUG if __import__("os").environ.get("ITERM_HINTS_DEBUG") else logging.INFO,
    format="%(asctime)s %(message)s",
)
log = logging.getLogger("iterm-hints")

sys.path.insert(0, str(Path(__file__).resolve().parent))
from hints_core import Config, Target, find_targets, load_config, make_hints  # noqa: E402

UV = next(
    (p for p in (shutil.which("uv"), "/opt/homebrew/bin/uv", "/usr/local/bin/uv", str(Path("~/.local/bin/uv").expanduser())) if p and Path(p).exists()),
    "uv",
)
OVERLAY = Path(__file__).resolve().parent / "overlay.py"
MODIFIERS = {
    "cmd": iterm2.Modifier.COMMAND,
    "opt": iterm2.Modifier.OPTION,
    "alt": iterm2.Modifier.OPTION,
    "ctrl": iterm2.Modifier.CONTROL,
    "shift": iterm2.Modifier.SHIFT,
}
RELEVANT_MODS = set(MODIFIERS.values())


def parse_activation(spec: str) -> tuple[set, iterm2.Keycode]:
    *mods, key = spec.lower().split("+")
    name = {"space": "SPACE", "escape": "ESCAPE", "return": "RETURN"}.get(key, f"ANSI_{key.upper()}")
    return {MODIFIERS[m] for m in mods}, iterm2.Keycode[name]


async def read_viewport(connection, session) -> list[tuple[str, list[tuple[int, int]]]]:
    """Return logical lines (soft wraps joined) as (text, [(row, col) per char])."""
    async with iterm2.Transaction(connection):
        info = await session.async_get_line_info()
        rows = session.grid_size.height
        lines = await session.async_get_contents(info.first_visible_line_number, rows)
    cols = session.grid_size.width
    out: list[tuple[str, list[tuple[int, int]]]] = []
    text, pos = "", []
    for row, line in enumerate(lines):
        for x in range(cols):
            try:
                cell = line.string_at(x)
            except IndexError:
                cell = " "
            if cell == "" and x < len(line.string):
                continue  # continuation cell of a wide character
            cell = cell if cell not in ("", "\0") else " "
            for ch in cell:
                text += ch
                pos.append((row, x))
        if line.hard_eol or row == len(lines) - 1:
            out.append((text, pos))
            text, pos = "", []
    return out


def geometry(window_frame, session, tab_sessions, cfg: Config):
    """Return a function (row, col, ncols) -> (x, y, w, h) in Cocoa screen points."""
    f = session.frame
    tab_h = max(s.frame.origin.y + s.frame.size.height for s in tab_sessions)
    bottom = window_frame.origin.y + (tab_h - (f.origin.y + f.size.height))
    top = bottom + f.size.height - cfg.margin_y
    left = window_frame.origin.x + f.origin.x + cfg.margin_x
    cell_w = (f.size.width - 2 * cfg.margin_x) / session.grid_size.width
    cell_h = (f.size.height - 2 * cfg.margin_y) / session.grid_size.height

    def rect(row: int, col: int, ncols: int):
        return left + col * cell_w, top - (row + 1) * cell_h, ncols * cell_w, cell_h

    return rect


class Overlay:
    def __init__(self) -> None:
        self.proc = subprocess.Popen(
            [UV, "run", "--script", str(OVERLAY)], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True
        )

    async def wait_ready(self, timeout: float = 10.0) -> None:
        """Wait until the overlay window is on screen."""
        loop = asyncio.get_running_loop()
        try:
            await asyncio.wait_for(loop.run_in_executor(None, self.proc.stdout.readline), timeout)
        except asyncio.TimeoutError:
            log.warning("overlay not ready after %ss", timeout)

    def send(self, msg: dict) -> None:
        self.proc.stdin.write(json.dumps(msg) + "\n")
        self.proc.stdin.flush()

    def close(self) -> None:
        try:
            self.send({"quit": True})
            self.proc.stdin.close()
        except (BrokenPipeError, ValueError):
            pass
        self.proc.terminate()


def open_target(t: Target, cfg: Config) -> None:
    if t.kind == "file":
        mapping = {"path": t.path, "line": t.line or "1", "col": t.col or "1"}
        template = cfg.open_file_command
    else:
        mapping = {"url": t.url}
        template = cfg.url_command
    subprocess.Popen([a.format(**mapping) for a in shlex.split(template)])


def collect_targets(logical_lines, cfg: Config, cwd: str | None):
    """Return [(Target, row, col, ncols)] with hint labels assigned."""
    found = []
    for text, pos in logical_lines:
        for t in find_targets(text, cfg, cwd):
            row, col = pos[t.start]
            end_row, end_col = pos[t.end - 1]
            ncols = (end_col + 1 - col) if end_row == row else (max(c for r, c in pos if r == row) + 1 - col)
            found.append((t, row, col, ncols))
    for (t, *_), label in zip(found, make_hints(len(found), cfg.alphabet)):
        t.label = label
    return found


def badges(found, typed: str, rect):
    out = []
    for t, row, col, ncols in found:
        if t.label.startswith(typed):
            x, y, w, h = rect(row, col, ncols)
            out.append({"x": x, "y": y, "w": w, "h": h, "label": t.label, "typed": typed})
    return out


async def hint_mode(connection, app, mon, cfg: Config) -> None:
    window = app.current_terminal_window
    tab = window.current_tab if window else None
    session = tab.current_session if tab else None
    if session is None:
        return
    await app.async_refresh()
    cwd = await session.async_get_variable("path")
    found = collect_targets(await read_viewport(connection, session), cfg, cwd)
    if not found:
        return
    rect = geometry(await window.async_get_frame(), session, tab.all_sessions, cfg)
    wf = await window.async_get_frame()
    frame = [wf.origin.x, wf.origin.y, wf.size.width, wf.size.height]

    keys = iterm2.KeystrokePattern()
    keys.keycodes = list(iterm2.Keycode)
    overlay = Overlay()
    typed = ""
    try:
        overlay.send({"frame": frame, "badges": badges(found, typed, rect)})
        await overlay.wait_ready()
        await app.async_activate(raise_all_windows=False, ignoring_other_apps=True)
        async with iterm2.KeystrokeFilter(connection, [keys], session.session_id):
            while True:
                ks = await mon.async_get()
                if ks.keycode == iterm2.Keycode.ESCAPE:
                    return
                if ks.keycode == iterm2.Keycode.DELETE:
                    typed = typed[:-1]
                else:
                    ch = ks.characters_ignoring_modifiers.upper()
                    if len(ch) != 1 or ch not in cfg.alphabet:
                        continue
                    typed += ch
                matches = [f for f in found if f[0].label.startswith(typed)]
                if not matches:
                    typed = typed[:-1]
                    continue
                exact = [f for f in matches if f[0].label == typed]
                if exact:
                    open_target(exact[0][0], cfg)
                    return
                overlay.send({"badges": badges(found, typed, rect)})
    finally:
        overlay.close()


async def main(connection) -> None:
    cfg = load_config()
    want_mods, want_key = parse_activation(cfg.activation)
    app = await iterm2.async_get_app(connection)
    log.info("started, activation=%s", cfg.activation)

    pattern = iterm2.KeystrokePattern()
    pattern.required_modifiers = list(want_mods)
    pattern.keycodes = [want_key]
    async with iterm2.KeystrokeFilter(connection, [pattern]), iterm2.KeystrokeMonitor(connection) as mon:
        while True:
            ks = await mon.async_get()
            log.debug("key %r", ks)
            if ks.keycode == want_key and want_mods <= set(ks.modifiers) & RELEVANT_MODS:
                log.info("activation key seen")
                try:
                    await hint_mode(connection, app, mon, cfg)
                except Exception:  # keep daemon alive, record why
                    log.exception("hint mode failed")


if __name__ == "__main__":
    iterm2.run_forever(main)
