---
name: daily-setup
description: Configure where the daily skill stores standup entries.
disable-model-invocation: true
---

Configures the backend `daily` reads and writes. Never fires on its own — only runs when the user types it.

## Config file

`~/.claude/daily/config.json`. Shape:

```json
{
  "backend": "markdown",
  "markdown": { "path": "/absolute/path/to/dir" },
  "confluence": { "configured": false },
  "notes": { "configured": false }
}
```

`backend` names the active adapter. Only the active adapter's settings need to be filled in; leave the others at `{ "configured": false }`.

## Steps

1. Ask which backend to use: **markdown** (fully supported), **confluence** or **notes** (not implemented yet — pick only if the user explicitly wants to reserve the choice for later).
2. If **markdown**: ask for an absolute directory path. Create it if missing (`mkdir -p`). Write `config.json` with `backend: "markdown"` and that path.
3. If **confluence** or **notes**: write `config.json` with that backend name active but its settings left `{ "configured": false }`. Tell the user plainly that this adapter has no working implementation yet, so `daily` will refuse to write until it's built.
4. Confirm back to the user what was written and where.

## Completion criterion

`~/.claude/daily/config.json` exists and its `backend` field matches what the user picked.
