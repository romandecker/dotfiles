# dotfiles

My personal dotfiles and machine setup — Mac-first, with best-effort Linux (CLI)
support. Managed by [chezmoi](https://www.chezmoi.io/).

## What is chezmoi (and why)?

chezmoi is a single, dependency-free binary that turns this repo into your
`$HOME`. It keeps a **source directory** (the `home/` folder here) and, on
`chezmoi apply`, materialises those files into your home directory. It replaced
an older pile of hand-rolled `bash` installers.

Two things make it worth learning:

- **One-command bootstrap** on a brand-new machine (below), including installing
  Homebrew and every package.
- **Per-OS config from one source** via Go templates (`.tmpl` files), so Mac and
  Linux can differ without maintaining two forks.

This repo runs chezmoi in **symlink mode**: most files in `$HOME` become symlinks
back into this repo, so editing e.g. `~/.vimrc` edits the repo copy live — just
like a classic symlink-based dotfiles setup. Files that *can't* be symlinks
(templates, executables) are written as real files.

## Bootstrap a new machine

```sh
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply romandecker
```

That one line:

1. downloads the `chezmoi` binary,
2. clones this repo to `~/.local/share/chezmoi`,
3. writes `~/.config/chezmoi/chezmoi.toml` (turns on symlink mode),
4. runs the package installer (installs Homebrew if missing, then `brew bundle`
   the [Brewfile](home/Brewfile); on Linux, a small `apt` set),
5. clones third-party externals (tpm, zgen, tmuxifier, vim-plug),
6. links every dotfile into `$HOME` and runs the post-apply scripts (git config,
   fzf key-bindings, neovim plugins, the Claude statusline).

To preview what it *would* do without changing anything: add `--dry-run` and/or
run `chezmoi diff` afterwards.

## How it's organized

chezmoi uses **filename prefixes** to decide what a source file becomes in
`$HOME`. The important ones here:

| Source name (in `home/`)                    | Becomes / does                                              |
| ------------------------------------------- | ---------------------------------------------------------- |
| `dot_vimrc`                                 | `~/.vimrc` (the `dot_` prefix = a leading `.`)             |
| `dot_tmux.conf.tmpl`                        | `~/.tmux.conf`, rendered from a template (per-OS logic)    |
| `dot_local/bin/executable_git-browse-pr`    | `~/.local/bin/git-browse-pr`, marked executable           |
| `run_onchange_before_10-install-packages.sh.tmpl` | script run when its contents change, *before* files  |
| `run_once_after_40-nvim-plug.sh`            | script run exactly once, *after* files are applied         |
| `.chezmoiexternal.toml`                     | declares third-party repos/files to fetch & auto-update    |
| `.chezmoiignore`                            | source files that should *not* be applied to `$HOME`       |
| `.chezmoi.toml.tmpl`                        | generates your chezmoi config (this is where symlink mode lives) |
| `Brewfile`                                  | package list consumed by the install script (not a dotfile) |

`.chezmoiroot` at the repo root points chezmoi at `home/`, which keeps repo docs
(this README, `MIGRATION.md`, `test/`) out of your home directory.

### The symlink-leaning model

Because of `mode = "symlink"`, editing a managed dotfile edits this repo directly
— no `chezmoi apply` needed for the change to take effect. The exception is
`.tmpl` files (e.g. `dot_tmux.conf.tmpl`): those are *rendered* to real files, so
after editing the template run `chezmoi apply` to regenerate the target.

## Daily workflow

```sh
chezmoi edit ~/.zshrc     # edit a managed file (opens the source)
chezmoi diff              # preview pending changes
chezmoi apply             # apply changes (needed after editing a .tmpl)
chezmoi update            # pull latest from git and apply
chezmoi cd                # drop into the source dir (home/)
```

Add a package: edit [home/Brewfile](home/Brewfile), then `chezmoi apply` (the
install script re-runs because the Brewfile's hash changed).

Add a new dotfile to management: `chezmoi add ~/.somerc`.

## Platform support

- **macOS** — primary, tested. Full Brewfile.
- **Linux (CLI)** — best-effort. A small `apt` package set; templates guard
  mac-only bits. Not continuously tested — expect to fix a rough edge if you use
  it. (Linux-desktop config — i3, xkb — was intentionally dropped.)

## What isn't managed yet

See [MIGRATION.md](MIGRATION.md) for the tracked backlog (GUI-app config, macOS
`defaults`, the emacs configs pending a cull, etc.).
