---
name: reconcile-branch
description: Bring the current branch up to date with its base branch (the PR target, or the default branch). Choose rebase or merge, resolve conflicts, and push. Works with any Git host.
disable-model-invocation: true
---

# Reconcile Branch

Update the current branch with its base branch. Resolve conflicts. Push the result.

The user can pass a base branch as an argument. Use it with `--base` and skip the PR lookup.

## Helper scripts

Both scripts are on `PATH`. They do the mechanical work. Do not repeat their work with raw git commands.

- `reconcile-plan [--base <branch>]` fetches, checks the start state, finds the base, and picks the strategy. It prints JSON. Read its `status` field.
- `reconcile-run merge|rebase <base>` runs the merge or rebase. It prints JSON. `status` is `ok` or `conflict` (with a `files` list). Exit code 2 means conflict.
- `reconcile-run --continue` finishes the merge or rebase after you resolved and staged the conflicted files.
- `reconcile-run --push` pushes. It uses `--force-with-lease` after a rebase and a plain push after a merge. No confirmation is needed for the force push.

## Rules

- Always use the latest base from the remote. The scripts run `git fetch origin` first and use `origin/<base>`. Never use the local base branch.
- Use the scripts for fetch, base detection, strategy, merge, rebase, and push.
- Never push a guess. If you are not sure about a conflict, stop and ask the user.
- Never use `git add -A` or `git add .`. Stage only the conflicted files you resolved.
- Do not invent a merge message. The script keeps the Git default.
- Ask one question at a time.

## Process

### 1. Plan

Run `reconcile-plan`, with `--base <arg>` if the user gave a base. Act on `status`:

- `blocked`: tell the user the `reason` and stop. Do not stash or discard anything.
- `up-to-date`: tell the user and stop. Do not push.
- `ask-base`: the base is unclear. Ask the user for the base branch. Then run `reconcile-plan --base <answer>`.
- `needs-pr-lookup`: go to step 2.
- `ready`: go to step 3.

### 2. Find the PR target

Use `kind` and the tools you have:

- **bitbucket-server:** the `bitbucket-server` MCP tools. Call `list_pull_requests` with the current branch as source. Use open pull requests only.
- **gitlab:** `glab mr view`.
- **other:** ask the user for the base.

Then:

- One PR found: run `reconcile-plan --base <target branch>`.
- No PR found: run `reconcile-plan --base <default_branch>`. If `default_branch` is empty, ask the user for the base.
- Several PRs with different targets: ask the user which target to use. Then run `reconcile-plan --base <answer>`.

### 3. Choose the strategy

Read `strategy` from the plan:

- `merge` or `rebase`: use it. Tell the user the `reason` in one sentence.
- `ask`: ask with the `AskUserQuestion` tool. Use a selection, not free text. Put the commit count in the question. Options:
  - "Rebase (Recommended)": replay the commits on the base. Needs a force push.
  - "Merge": add a merge commit. Keeps history and no force push.

### 4. Run

Run `reconcile-run <strategy> <base>`.

- `ok`: go to step 6.
- `conflict`: go to step 5.
- `error`: show the message and stop.

### 5. Resolve conflicts

If the `resolving-merge-conflicts` skill is available, use it for the files in the list. Otherwise resolve them by hand: read each file, keep the intent of both sides, remove all markers.

Stage the resolved files by name. Run `reconcile-run --continue`.

- `ok`: go to step 6.
- `conflict` again: a rebase hit the next commit. Repeat this step.
- Any doubt about a resolution: stop and ask the user. Show the file and both sides.

If the user wants to give up, run `git rebase --abort` or `git merge --abort`.

### 6. Push

Run `reconcile-run --push`. Report the strategy used, the base, and that the push worked.
