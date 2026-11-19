---
name: address-review-comments
description: Walk through the open review comments on the pull request of the current branch. Propose a fix for each one, commit accepted fixes one by one, post a reply with the commit SHA, and push once at the end. Works with any Git host (Bitbucket, GitHub, GitLab).
disable-model-invocation: true
---

# Address Review Comments

Go through the unaddressed review comments on the current branch's pull request. Handle them one at a time. The user decides on each one.

The user can pass a PR URL or number as an argument. Use it instead of looking for the PR.

## Helper scripts

Both scripts are on `PATH`.

- `pr-context` prints JSON about the checkout: host kind, project, repo, branch, dirty flag, ahead and behind counts, and the ticket key from the branch name.
- `edit-draft [--append-commit] <file>` opens `$EDITOR` in a split pane. It waits for the editor to close. It prints the final text on stdout. Exit code 3 means the user aborted (empty file or editor exit with an error). Exit code 4 means no editor could open. With `--append-commit` it adds `<short sha> <subject>` of HEAD to the draft first.

The Bash tool has no TTY. Never run `nvim` or `vim` directly. Always use `edit-draft`.

## Rules

- Commits follow the rules that apply in this session. This skill adds none. Do not invent a message format or a ticket key.
- Never force-push.
- Never use `git add -A` or `git add .`. Stage only the files you changed for the fix.
- Ask the user one question at a time.
- Show one thread at a time. Never show a list of threads, except the final report.
- Use plain chat for questions. Do not use a multiple-choice UI.

## Process

### 1. Check the start state

Run `pr-context`. If `dirty` is true, stop. Show `git status --short` and ask the user what to do. Do not stash or discard anything without an answer.

If `behind` is above 0, tell the user. Ask before you pull.

### 2. Find the PR

Use the `kind` field and the tools you have. Pick the first one that works:

- **bitbucket-server:** the `bitbucket-server` MCP tools. `list_pull_requests` with the branch as the source, then `get_pull_request_activity` for the comments.
- **github:** `gh pr view --json number,url` for the PR. Read the review threads with `gh api graphql` (`reviewThreads` with `isResolved`, `isOutdated`, and the comments). Also read the issue comments.
- **gitlab:** `glab mr view` and `glab api` for discussions.
- **unknown or no tool works:** ask the user how to read the PR. Offer to take pasted comments.

If the PR does not exist, stop and say so. Do not create a PR.

Find out who the current user is on that host (for example `gh api user`, or the MCP user profile). You need it for step 3.

### 3. Select the comments

A thread is in scope when all of these hold:

- It is not resolved.
- Its root comment is by someone else.
- The last reply is not by the user, or the reviewer answered after the user's reply.
- It is attached to code, or it is a general comment that asks for an action.

Include outdated threads. Mark them "possibly outdated" and check if the code still has the problem.

Comments with no action, such as "LGTM" or "nice", are not in scope. List them at the end as "no action found".

Keep this list to yourself. Do not show the threads, their text, or your plans for them. Tell the user only how many threads are in scope. Then start step 4 with the first one.

If no thread is in scope, say so and stop.

### 4. Walk the threads

Work on exactly one thread at a time. Take the threads in file and line order.

Do not show, summarize, analyze, or plan for any other thread until you finish the current one. Do not read code for the next thread early. "Finish" means the thread is committed and replied to, answered, or skipped.

For the current thread:

1. Check the other in-scope threads for one about a VERY similar thing: the same problem, in another place, or the same request made again. If you find one, tell the user in one or two sentences. Name the other thread by file, line, and author. Ask whether to handle both together. Do not do this for loosely related threads. If the user says no, handle them one at a time. If the user says yes, treat them as one unit: one fix, one commit, and a separate reply for each thread (step 6 runs once per thread, with the same SHA).
2. Show the reviewer's text, the author, and the code around the anchor (read the file, do not guess).
3. Propose a solution. For a code change, show the diff you would make as text. Do not edit files yet. If no code change is needed, propose an answer.
4. Ask for a choice:
   - **Accept:** go to step 5.
   - **Adjust:** the user says what to change. Revise the proposal and ask again.
   - **Reply only:** no code change. Go to step 6 with an answer and no SHA.
   - **Skip:** leave the thread alone. Go to the next one.
   - **Stop:** go to step 7.

After step 6 or a Skip, go back to the start of step 4 with the next thread.

### 5. Implement and commit

1. Implement the fix with the project's normal flow. Run the checks the project uses.
2. Stage only the files you changed. Commit once for this thread, or once for the whole group if the user merged threads. Use the commit rules of this session.
3. Go to step 6.

### 6. Draft, edit, post

1. Write the draft to a file in the scratchpad directory. Use this form:

   ```
   <answer to the comment>

   <short sha> <first line of the commit message>
   ```

   - Leave out the answer line if the fix explains itself.
   - For a reply with no code change, write the answer only.
   - Write like the user: short, direct, informal is fine.
   - Examples of replies:
     - `Indeed it was best effort. Added your suggestions`
     - `Addressed in`
     - `yeah you're right, this is claude doing bullshit tests 😛`

2. Run `edit-draft --append-commit <file>` when a commit exists. Run `edit-draft <file>` when none exists. The SHA line is added to the file by the script, so do not write it yourself.
3. If it exits with 0, post stdout as a reply on the thread with the host's tool.
4. If the host can resolve threads, ask "Mark as resolved?" and do it on yes.
5. If posting is not possible (no tool), print the final text and tell the user to post it by hand.

If `edit-draft` exits with 3 (abort), post nothing. Ask the user:

- If a commit exists: keep it or undo it? Undo only after the user says yes (`git reset --hard HEAD~1` is safe here, because you made this commit and the tree was clean).
- Then: skip this thread, or try again? Try again means go back to step 4 for the same thread.

If `edit-draft` exits with 4, show the draft in chat and ask for edits there.

### 7. Push and report

After the last thread, or after Stop:

1. If you made no commits, skip the push.
2. Otherwise run `git push`. If the branch has no upstream, ask before you set one. If the push is rejected, stop and ask. Never force-push.
3. Report:
   - Threads fixed and committed (with SHAs).
   - Threads answered without a code change.
   - Threads skipped, and threads not reached after Stop.
   - Threads with "no action found".

The replies are posted before the push. Their SHAs work only after the push. Push as soon as the last thread is done.
