# Migration backlog (bash installers → chezmoi)

Phase 1 (core dotfiles + Brewfile + `bin/`) is done. This file tracks what's
left, the judgment calls made along the way, and what was intentionally dropped.

## ✅ Done in Phase 1

- chezmoi scaffold: `.chezmoiroot` → `home/`, `mode = "symlink"`.
- Brewfile replacing the `ensure`/`brew`/`apt` installer logic.
- Core dotfiles migrated: zsh, vim/nvim, tmux (merged into one template), git,
  the `zsh-lib`/`.zfunctions`/`snippets`/`hygen-templates` support dirs.
- `bin/` scripts → `~/.local/bin` (managed, executable).
- Submodules retired → Homebrew (`pure`, `tfenv`, `zsh-syntax-highlighting`, `z`,
  fonts) or `.chezmoiexternal.toml` (tpm, zgen, tmuxifier, vim-plug).
- Claude statusline (scripts + settings merge).
- `run_*` scripts for git config, fzf, neovim plugins, macOS press-and-hold.

## ✅ Done (post-Phase-1)

- **VSCode** — the old installer symlinked the *entire* `~/Library/Application
  Support/Code/User` dir at the repo root (`vscode/`), so VSCode wrote 668 MB of
  runtime state (History, globalStorage, workspaceStorage) into the repo. Now:
  the User dir is a real directory again (runtime state stays local, unmanaged);
  chezmoi manages only `settings.json`, `keybindings.json`, and `snippets/*.json`
  (mac path, symlink mode); the 31 extensions are captured as `vscode "…"` entries
  in the Brewfile so `brew bundle` reinstalls them. Dropped empty `mcp.json` /
  `chatLanguageModels.json` / `agent-sessions.code-workspace` and a stale
  `atlascode` schema path. Mac-only by design (GUI app; Linux is CLI-only).

- **iTerm2** — the whole default profile is now reproducible, not just colors:
  - **Profile** captured as a Dynamic Profile JSON
    (`private_Library/private_Application Support/iTerm2/DynamicProfiles/default.json`,
    symlinked into `~/Library/Application Support/iTerm2/`). iTerm reads it
    natively, so it's a plain managed file — no run-script fighting iTerm's plist.
    Two machine-dependent values were scrubbed: `Working Directory`
    (`/Users/romande`, and inert anyway since `Custom Directory = No`) was dropped,
    and `Normal Font` was switched from the hand-installed `DejaVuSansMonoPowerline`
    to `Agave Nerd Font 12` (already a Brewfile cask, keeps powerline/nerd glyphs).
  - **Color presets**: `iterm-color-themes/*.itermcolors` (currently `Alabaster`,
    exported from the live plist; the stale 2019 `material` was retired) are merged
    into the plist's "Custom Color Presets" so they stay selectable in
    Preferences → Colors. Sources live source-only (in `.chezmoiignore`).
  - The Dynamic Profile is given its **own fresh GUID**
    (`B510869E-…`), deliberately *not* the captured profile's original GUID
    (`A5A03B86-…`). iTerm refuses a Dynamic Profile whose GUID matches an existing
    non-dynamic profile ("conflicts with non-dynamic profile with same Guid"), so
    reusing the captured GUID is a hard error, not a cosmetic one.
  - `run_onchange_after_55-iterm.sh.tmpl` (macOS-only) sets `Default Bookmark Guid`
    to the profile's GUID (read out of the JSON, so there's one source of truth)
    and does the preset merge; it re-runs when the profile JSON or any theme file
    changes, and tolerates a missing prefs file (fresh machine, iTerm never
    launched). ⚠️ It writes iTerm's prefs then flushes `cfprefsd` — iTerm rewrites
    that plist on quit, so apply with **iTerm closed**. On the *capture* machine
    the original non-dynamic "Default" profile (`A5A03B86-…`) lingers alongside the
    new dynamic one; it's redundant and can be deleted by hand in iTerm's prefs. A
    fresh machine never has it.

## ⏳ Deferred (still to migrate)

### macOS system defaults
- Only the VSCode `ApplePressAndHoldEnabled` tweak was migrated. A broader
  `defaults write` script (keyboard repeat, Finder, Dock…) is a fresh capability
  to add in `run_onchange_after_50-macos-defaults.sh.tmpl`.

### Other unmigrated dotfiles
- `.unison/dropbox-drive.prf` — unison sync profile (unison is in the Brewfile).
- `.tmux-layouts/` — gitignored, work-specific tmuxifier sessions.
- `test256colors.py`, `TODO.org` — utility/notes, left at root.

## 🗑️ Intentionally dropped
- **BetterTouchTool** (`better-touch-tool-preset.json`) and **flycut** (was a
  Brewfile cask) — both no longer used, so neither the preset nor the cask is
  managed anymore. `brew bundle` won't reinstall flycut on a fresh machine; it is
  not auto-uninstalled from machines that already have it.
- All emacs configs: `.doom.d`, `.emacs.d`, `.spacemacs`, `.emacs-profiles.el`
  (doom + spacemacs + bespoke config + chemacs). Emacs is no longer used, so
  nothing was migrated. Last-good state is tagged `before-emacs-retirement`.
  Also dropped the emacs-based `magit` zsh alias, both `.dir-locals.el` hygen
  generators (`project/dir-locals` + the TS-library template), and the root
  `.gitignore` emacs-runtime block.
- `.vrapperrc` — Eclipse Vrapper (vim bindings); dead, no longer on Eclipse.
- Linux-desktop config: `.i3config`, `.i3blocks.conf`, `xkb-keyboard-layouts/`.
- Server flavor: `web-install-server.sh`, `server.vimrc`.
- `web-install.sh` (replaced by the chezmoi bootstrap one-liner).
- `yvm` (abandoned). **nvm is unchanged** — still the `zsh-nvm` zgen plugin +
  `~/.nvm`, *not* brew nvm (brew nvm would double-init and fight the plugin).
- Vendored `rupa/z` copy: retired outright. Directory-jumping already comes from
  the `agkozak/zsh-z` zgen plugin, so there's no brew `z` replacement.
- GNU `readlink` shim: the old `ln -fs $(which greadlink) /usr/local/bin/readlink`
  is gone (it hardcoded the Intel brew path anyway). `coreutils` still provides
  `greadlink`/`grealpath`; if a script needs GNU `readlink` as `readlink`, add a
  `g`-prefixed call or a PATH shim for `$(brew --prefix)/opt/coreutils/libexec/gnubin`.
- `wakatime` installer (was a self-defeating no-op) and `grip`/`mermaid.cli`
  (manual/deprecated) — re-add deliberately if still wanted.

## 📝 First-apply gotchas (this machine only)
- **Font casks vs. legacy manual fonts**: the old `installer/008-fonts.sh` dropped
  FiraCode/Agave TTFs into `~/Library/Fonts` by hand. `brew bundle` can't adopt
  files whose bytes differ, so `font-fira-code`/`font-agave-nerd-font` failed on
  first apply. Resolved once with `brew install --cask --force font-fira-code
  font-agave-nerd-font` (brew now owns them). Fresh machines don't hit this — no
  `force` is baked into the Brewfile.

## 📝 Known caveats (verify when you next use them)
- **tmuxifier layout path** (resolved on the first migrated machine):
  `TMUXIFIER_LAYOUT_PATH` is `~/.tmux-layouts`. tmuxifier wants BOTH `*.session.sh`
  and `*.window.sh` in that one dir, so work sessions were moved there from
  `~/.dotfiles/.tmux-layouts` and the generic `*.window.sh` layouts copied in
  alongside. The canonical window layouts still live in the repo
  (`home/dot_tmuxifier/layouts/` → `~/.tmuxifier/`); the copies in `~/.tmux-layouts`
  are what tmuxifier actually reads, so edit the repo copy and re-copy on change.
  A fresh machine starts with an empty `~/.tmux-layouts` (sessions are machine-local).

## ⚠️ Judgment calls to review before you `chezmoi apply`
- **zsh decoupling**: `.zshrc` was rewritten to source from `$HOME` locations and
  brew paths instead of `$DOTFILES_DIR`/submodules. `pure`,
  `zsh-syntax-highlighting`, `nvm`, `tfenv` now come from brew. Verify the prompt
  and completions still load the way you like.
- **`bin/` scripts** are copied executables (symlink mode can't symlink
  executables), so edits need `chezmoi apply`. Move a script back to live-symlink
  by dropping its `executable_` prefix if you edit it often.
- **git config** is applied imperatively (`git config --global`) so your identity
  in `~/.gitconfig` is never clobbered.
