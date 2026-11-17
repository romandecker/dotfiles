---
name: to-fp-tickets
description: Break a plan, spec, or the current conversation into a set of tracer-bullet tickets, each declaring its blocking edges, and publish them to zooplus Jira's Frontend Platform (FP) project via the Atlassian MCP.
disable-model-invocation: true
---

# To FP Tickets

Break a plan, spec, or conversation into a set of **tickets**: tracer-bullet vertical slices, each declaring the tickets that **block** it, published as Jira issues in the zooplus **Frontend Platform** project (key `FP`, https://zooplus.atlassian.net/jira/).

This skill is self-contained: it always targets zooplus Jira's FP project. It never needs `setup-matt-pocock-skills` or any other configuration skill. Use the Atlassian MCP for every Jira interaction.

## Process

### 1. Gather context

Work from whatever is already in the conversation context. If the user passes a reference (a spec path, a Jira issue key or URL) as an argument, fetch it and read its full body and comments.

If the source references an existing Jira issue (an epic, a parent story), note its key: new tickets will set it as their `Parent`.

Keep every URL the source mentions (docs, RFCs, Confluence pages, PRs, tickets). The descriptions must link to them.

## Linking rules

Use real links wherever possible. These rules apply to every description and to the final report.

- Outside documentation: use a Markdown link with a clear label, for example `[Next.js caching](https://nextjs.org/docs/app/building-your-application/caching)`. Never write "see the docs" without a link.
- Other tickets: use the full Jira URL, `https://zooplus.atlassian.net/browse/FP-123`. Hyperlink the key in the description, for example `[FP-123](https://zooplus.atlassian.net/browse/FP-123)`.
- Do not invent URLs. Link only to pages you fetched, searched, or received from the user. If you are unsure of a URL, look it up or leave the link out.

### 2. Explore the codebase (optional)

If you have not already explored the codebase, do so to understand the current state of the code. Ticket titles and descriptions should use the project's domain glossary vocabulary, and respect ADRs in the area you're touching.

Look for opportunities to prefactor the code to make the implementation easier. "Make the change easy, then make the easy change."

### 3. Draft vertical slices

Break the work into **tracer bullet** tickets.

<vertical-slice-rules>

- Each slice cuts a narrow but COMPLETE path through every layer (schema, API, UI, tests). Vertical, NOT a horizontal slice of one layer
- A completed slice is demoable or verifiable on its own
- Each slice is sized to fit in a single fresh context window
- Any prefactoring should be done first

</vertical-slice-rules>

Give each ticket its **blocking edges**: the other tickets that must complete before it can start. A ticket with no blockers can start immediately.

**Wide refactors are the exception to vertical slicing.** A **wide refactor** is one mechanical change, such as renaming a column or retyping a shared symbol, whose **blast radius** fans across the whole codebase, so a single edit breaks thousands of call sites at once and no vertical slice can land green. Don't force it into a tracer bullet; sequence it as **expand-contract**. First expand: add the new form beside the old so nothing breaks. Then migrate the call sites over in batches sized by blast radius (per package, per directory), each batch its own ticket blocked by the expand, keeping CI green batch to batch because the old form still exists. Finally contract: delete the old form once no caller remains, in a ticket blocked by every migrate batch. When even the batches can't stay green alone, keep the sequence but let them share an integration branch that all block a final integrate-and-verify ticket. Green is promised only there.

### 4. Pick an issue type per ticket

Default every ticket to **Task**. Ask the user instead of guessing when a ticket clearly doesn't fit:

- Looks like a defect fix, not new behaviour: **Bug**
- Has a security-hardening focus: **Security Task**
- Is itself a container for the whole effort (e.g. the wide-refactor umbrella): ask whether it should be an **Epic**
- Doesn't fit Task/Bug/Security Task/Epic: ask, offering **Support** and **Technical Story** as the remaining valid types

Don't set any status. Leave the ticket on Jira's default workflow status (e.g. "To Do") for the type.

### 5. Find a matching epic per ticket

If step 1 gave a parent, use it for every ticket it fits. For each remaining ticket, search for a matching epic:

1. Search open epics in FP via the Atlassian MCP, for example JQL `project = FP AND issuetype = Epic AND statusCategory != Done ORDER BY updated DESC`.
2. Compare each epic's summary and description with the ticket's goal. Use keywords from the ticket and the domain glossary for extra searches when the first list shows no clear match.
3. Propose the best match with a one-line reason and the epic's link. If no epic fits, say so. Do not force a weak match.

Tickets of one effort usually share one epic. Different tickets can still map to different epics.

### 6. Quiz the user

Present the proposed breakdown as a numbered list. For each ticket, show:

- **Title**: short descriptive name
- **Type**: the Jira issue type chosen for it
- **Epic**: the proposed parent epic as a link, or "none found"
- **Blocked by**: which other tickets (if any) must complete first
- **What it delivers**: the end-to-end behaviour this ticket makes work

Ask the user:

- Does the granularity feel right? (too coarse / too fine)
- Are the blocking edges correct? Does each ticket only depend on tickets that genuinely gate it?
- Are the issue types right?
- Are the proposed epics right?
- Should any tickets be merged or split further?
- Should I assign the tickets to you now? (all, some, or none)
- Should I move the tickets into the current sprint? (all, some, or none)

Iterate until the user approves the breakdown and answers the assignment and sprint questions.

### 7. Publish the tickets to Jira

Publish the approved tickets to the **FP** project, one Jira issue per ticket, in dependency order (blockers first) so each ticket's blocking edges can reference real issue keys.

For each ticket, via the Atlassian MCP:

1. Create the issue in project `FP` with the chosen issue type, using the <issue-template> below for the summary and description.
2. Set the `component` field to `frontend-platform`. Every ticket this skill creates must carry this component, never skip it.
3. Apply the label `ai-generated`. Every ticket this skill creates must carry this label, never skip it.
4. Set Jira's native `Parent` field to the epic or parent chosen in steps 1 and 5 (and approved in step 6). Omit it only when none was found.
5. If the user asked for assignment in step 6, assign the ticket to the user (look up their account ID with the Atlassian MCP, for example via `atlassianUserInfo`). Otherwise leave it unassigned.
6. If the user asked for sprint placement in step 6, find the active sprint of the board that serves FP and add the ticket to it. Use the sprint field the MCP exposes. If the MCP cannot set the sprint, say so in the report and give the sprint name, so the user can move the tickets by hand.
7. For each ticket this one is blocked by, add a native Jira issue link of type **Blocks**, created from this (the blocked) ticket as "is blocked by" the blocker. The blocker ticket must already exist, since this is why publishing proceeds in dependency order.
8. For any other ticket mentioned in this ticket's description that isn't already covered by a Blocks link, hyperlink it (see Linking rules) and add a native issue link to it. Use the link type that best matches the relationship; default to **Relates to** when nothing more specific fits.

Do not apply any labels beyond `ai-generated`. Do not use text-based "Blocked by" sections in the description: the native issue link is the only source of truth for blocking edges.

Work the **frontier**: any ticket whose blockers are all done. For a purely linear chain that means top to bottom.

Do NOT close or modify any parent issue.

After creating each issue, call `getJiraIssue` and confirm `labels` contains `ai-generated` and `component` contains `frontend-platform`. If either is missing, fix it immediately via `editJiraIssue` before moving to the next ticket.

<issue-template>

**Summary:** <Ticket title>

**Description:**

<One-line motivation: the current state and why it must change. No heading. This is the very first line.>

## What to build

The end-to-end behaviour this ticket makes work, from the user's perspective, not layer-by-layer implementation.

## Acceptance criteria

- Criterion 1
- Criterion 2

## References

Optional. Links to outside documentation and related tickets, as Markdown links. Omit when there are none.

</issue-template>

Rules for the motivation line:

- It is one sentence. It states the current state and the reason to change it.
- It comes before any heading.
- Do not name the parent or epic in the description. The `Parent` field and the issue links already show it.

Avoid specific file paths or code snippets in the description: they go stale fast. Exception: if a prototype produced a snippet that encodes a decision more precisely than prose can (state machine, reducer, schema, type shape), inline it and note briefly that it came from a prototype. Trim to the decision-rich parts, not a working demo, just the important bits.

### 8. Report back

Once published, give the user the list of created issues in the same dependency order they were created. For each, show the Jira key as a link (`[FP-123](https://zooplus.atlassian.net/browse/FP-123)`), the title, the epic link, the assignee, and the sprint. Flag any ticket where assignment or sprint placement failed.
