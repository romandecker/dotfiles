---
name: daily
description: Track a completed piece of work for the next standup, or show/edit the day's tracked entries. Use when the user finishes a task, lands a commit, closes a ticket, or explicitly asks to add something to today's daily, show the daily/standup summary, or fix the last daily entry.
---

Keeps a running log of work worth mentioning at the next standup, stamped one entry per calendar day. Branches on intent: **add**, **show**, or **edit/remove latest**. Read the invocation to pick the branch; when genuinely ambiguous, ask.

## Setup gate

Before anything else, check `~/.claude/daily/config.json` exists and its `backend` is `markdown` with `markdown.path` set (the only implemented backend today). If missing or another backend is active without an implementation: tell the user to run `daily-setup` first, and stop. Do not create a default location yourself.

## Storage shape

One file per calendar day in `markdown.path`, named `YYYY-MM-DD.md`. Each entry is one line:

```
- HH:MM [source] one-liner
```

`source` is one of `session`, `ticket`, `commit`, `manual`. `one-liner` is phrased for the standup, not a raw log dump, e.g. "Fixed off-by-one bug in retry loop" not "changed line 42 of retry.go".

## Branch: add

1. Compress whatever prompted this into one standup-ready line. Pull the source tag from context (current git commit/ticket being worked, or `manual` if just dictated).
2. If this was triggered by the user explicitly asking to track something, write immediately. If Claude decided on its own to suggest tracking it (no explicit ask from the user in this turn), show the proposed one-liner and ask for confirmation before writing.
3. Append the line to today's `YYYY-MM-DD.md` (create the file with a `# Daily YYYY-MM-DD` header if it doesn't exist yet).

## Branch: show

1. List files in `markdown.path`, sorted by filename descending (filenames sort chronologically).
2. Pick the first file that has at least one entry line. That is "the most recent daily."
3. Print its date and every entry, at a glance, standup-ready.
4. If no file has any entries, say so plainly.

## Branch: edit/remove latest

1. Only operates on **today's** file (`YYYY-MM-DD.md` for the current date) — not any earlier bucket.
2. If today's file has no entries, say so and stop.
3. To edit: replace the last entry line with the corrected one-liner, keeping its timestamp and source tag unless told otherwise.
4. To remove: delete the last entry line.

## Completion criterion

- add: the one-liner is appended to today's file (or the user declined the confirm prompt).
- show: every entry in the most recent non-empty bucket has been printed.
- edit/remove: today's last entry matches what the user asked for.
