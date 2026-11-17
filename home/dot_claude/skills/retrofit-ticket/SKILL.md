---
name: retrofit-ticket
description: Retrofit a quick task that was done without a ticket. Infer the task from the conversation and the uncommitted changes, create an FP ticket for it, move the changes to a new worktree on a branch named after the ticket, commit, push, and open a pull request.
disable-model-invocation: true
---

# Retrofit Ticket

The user did a quick task first and skipped the paperwork. The changes sit uncommitted in the current worktree, often on an unrelated branch. This skill lifts them into the normal model: one ticket, one branch, one worktree, one pull request.

The user can pass a hint as an argument, for example a title, an issue type, or the files that belong to the task. Use it to override what you infer.

## Conventions

- **Ticket:** zooplus Jira, project `FP`, created the way `to-fp-tickets` creates them.
- **Branch:** the bare ticket key, for example `FP-1234`. No prefix, no slug.
- **Base:** the remote default branch (`origin/master` in most repos).
- **Commit subject and PR title:** `<KEY> | <Imperative summary>`, for example `FP-1234 | Fix the cache key for locale routes`.

## Process

### 1. Collect the changes

Run these in the current worktree:

- `git status --porcelain`
- `git diff HEAD` (staged and unstaged together)
- the content of each untracked file that could belong to the task
- `git log --oneline @{upstream}..HEAD`, if the branch has an upstream

If the tree is clean and no unpushed commits exist, stop and tell the user there is nothing to retrofit.

Unpushed commits only count when the conversation shows they belong to the quick task. Ask before you include them.

### 2. Infer the task and its scope

Use the conversation so far and the diff to decide:

- **What the task is:** the problem, the change, and why it matters.
- **Which files belong to it.** The worktree can also hold unrelated work in progress. Leave that work alone.

Work at file level. If one file holds both task changes and unrelated changes, do not try to split hunks. Name the file and ask the user what to do: carry the whole file, leave it, or let the user split it by hand first.

### 3. Draft the ticket

Read `~/.claude/skills/to-fp-tickets/SKILL.md`. You cannot invoke that skill (it has `disable-model-invocation`), so follow its rules by hand for exactly one ticket:

- Skip its slicing steps (2 and 3). A quick task is one ticket.
- Pick the issue type with its step 4. A fix for broken behaviour is a **Bug**.
- Find an epic with its step 5.
- Write the summary and description with its `<issue-template>`, its motivation-line rules, and its linking rules. Base the acceptance criteria on what the diff actually does.

### 4. Confirm once

Show the user one summary and get approval before you write anything outside this conversation:

- **Files to move:** the list from step 2, plus any unpushed commits. Also list the changed files that stay behind.
- **Ticket:** type, title, epic link, and the full description.
- **Assignment and sprint:** propose "assign to me, put in the current sprint". A quick task is usually work the user does now.
- **PR title:** `<KEY> | <summary>`, with `<KEY>` still a placeholder.

Iterate until the user approves. After approval, run steps 5 to 9 without more questions, unless something fails.

### 5. Create the ticket

Follow step 7 of `to-fp-tickets` for the one ticket: component `frontend-platform`, label `ai-generated`, parent, assignee, sprint, and the check with `getJiraIssue` after creation. Note the new key.

### 6. Move the changes to a new worktree

1. Save a safety copy first. Write `git diff HEAD --binary -- <files>` to `<KEY>.patch` in the session scratchpad. If this step fails, stop.
2. Stash only the task files, untracked ones included:
   `git stash push --include-untracked -m "retrofit <KEY>" -- <files>`
3. Find the stash entry by its message in `git stash list`. Do not assume it is `stash@{0}`.
4. Create the worktree with `git worktree-go <KEY>`. It creates the branch from the remote default branch, opens a herdr workspace, and installs dependencies in the background. If it fails (for example, outside herdr), fall back to:
   `git fetch origin && git worktree add -b <KEY> ../<repo-dir>-<KEY> origin/<default-branch>`
5. Get the worktree path from `git worktree list --porcelain` (the entry with `branch refs/heads/<KEY>`).
6. Apply the stash there: `git -C <worktree> stash apply <stash-ref>`. The stash list is shared by all worktrees.
7. If the apply succeeds, drop the stash entry. If it conflicts, stop. Do not drop the stash. Tell the user which files conflict. This usually means the change depends on commits from the old branch that are not on the base yet. The stash and the patch file are still there for recovery.

For unpushed commits from step 1, use `git -C <worktree> cherry-pick <sha>...` before you apply the stash. Do not rewrite the old branch to remove them. Tell the user they are still on it.

### 7. Commit and push

In the new worktree:

- Stage only the moved files.
- Commit with the subject `<KEY> | <summary>`. In the body, write a short reason for the change in plain sentences.
- Push: `git -C <worktree> push -u origin <KEY>`.

Do not run the full test suite. CI does that. If the conversation already ran a check (lint, typecheck, a single test), you can run that same check again in the new worktree.

### 8. Open the pull request

Use the user's preferred method, in this order:

1. If the skill list contains a pull-request skill (for example `raise-pr`), invoke it for the new branch.
2. If the remote is Bitbucket Server, use the `bitbucket-server` MCP `create_pull_request` tool. Get the project key and repository slug from the `origin` URL. Target the default branch.
3. If the remote is GitHub, use `gh pr create`.

Title: `<KEY> | <summary>`. Description:

- The first line links the ticket: `[<KEY>](https://zooplus.atlassian.net/browse/<KEY>)`.
- Then a short paragraph about what changed and why.
- Then how it was tested, or "Not tested locally" if that is the truth.

### 9. Report

Tell the user:

- the ticket as a link, with its epic, assignee, and sprint
- the branch and the worktree path
- the pull request as a link
- what stays in the old worktree: unrelated changes, and any cherry-picked commits that are still on the old branch
- any step that failed or was skipped
