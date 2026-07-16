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

## ⏳ Deferred (still to migrate)

### GUI-app config
- `better-touch-tool-preset.json` — BetterTouchTool preset (manual import today).
- `iterm-color-themes/` — iTerm color scheme.
- `vscode/` — VSCode settings/keybindings/snippets. The extension list from the
  old `installer/013-vscode.sh` is **not** migrated (see below).
- `flycut` — installed via Brewfile; preferences not managed.

### macOS system defaults
- Only the VSCode `ApplePressAndHoldEnabled` tweak was migrated. A broader
  `defaults write` script (keyboard repeat, Finder, Dock…) is a fresh capability
  to add in `run_onchange_after_50-macos-defaults.sh.tmpl`.

### Editor sprawl — needs a cull decision (yours)
Left untouched at the repo root, **not** managed by chezmoi:
- `.doom.d`, `.emacs.d`, `.emacs-profiles.el`, `.spacemacs` — doom + spacemacs +
  a bespoke emacs config + chemacs. Decide which (if any) survive, then migrate
  the winner. chemacs/doom install steps were dropped for now.
- `.vrapperrc` — Eclipse vim bindings; likely dead.

### Other unmigrated dotfiles
- `.unison/dropbox-drive.prf` — unison sync profile (unison is in the Brewfile).
- `.tmux-layouts/` — gitignored, work-specific tmuxifier sessions.
- `test256colors.py`, `TODO.org` — utility/notes, left at root.

### VSCode extensions
The old installer force-installed ~20 extensions. Re-add as either a
`run_once_after_*.sh` calling `code --install-extension`, or (cleaner) a
`vscode "..."` block in the Brewfile.

## 🗑️ Intentionally dropped
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
