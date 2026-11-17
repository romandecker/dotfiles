# iterm-hints

Vimium-style link hints for iTerm2.

## Use

1. Enable the Python API: Settings > General > Magic > Enable Python API.
2. Run `chezmoi apply`. This installs the daemon and an AutoLaunch shim.
3. Restart iTerm2, or run the shim from the Scripts menu.
4. Press Ctrl+Opt+F. Yellow labels appear on URLs, JIRA IDs and file paths.
5. Type a label to open the target. Esc cancels. Backspace undoes one key.

## Config

Optional file: `~/.config/iterm-hints/config.toml`. It overrides these keys:
`activation`, `alphabet`, `jira_base_url`, `open_file_command`, `url_command`,
`margin_x`, `margin_y`, `matchers`. See `DEFAULT_CONFIG` in `hints_core.py`.

A matcher has `regex`, `type` (`url` or `file`) and `action`. The action is a
format string. `{0}` is the whole match, `{1}` is group 1, and so on.
`{jira_base_url}` is also available.

## Design

- `hints_core.py`: matchers, hint labels. No iTerm2 imports. Test: `python3 test_core.py`.
- `iterm_hints.py`: daemon. Reads the viewport, captures keys, opens targets.
- `overlay.py`: separate process. Draws a click-through window over iTerm2.

## Limits

- iTerm2 has no API for annotation removal or Semantic History.
  So labels use an overlay window, and files open through `open_file_command`.
- Label position comes from pane geometry. If labels look shifted, tune `margin_x` and `margin_y`.
- Only the active pane is scanned. Hints appear only for existing files.
