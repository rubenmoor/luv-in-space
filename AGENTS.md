# AGENTS.md

Luv in Space is a showcase for [luv](https://github.com/mbrock/luv), Mikael Brockman's Common Lisp GPU workshop: a hard sci-fi spaceflight game in near-future, realistic technology, centered on piloting a rugged DIY spaceship under Newtonian physics from low Earth orbit out into the solar system. It is the prelude to a space adventure with a linear quest line.

## Work: `pm`
- Milestones, plans, tasks, decisions: all in `tasks.sql`, only through `pm`. Read `pm instructions` first.
- Start: `pm status`, `pm show <task>`.
- Before a choice: `pm decision list --answered --grep <word>`; answers are binding.
- Human decisions are the exception: decide yourself and record it (`pm decision record`); the developer overrides by superseding.
  - `pm decision add` only for what no agent can decide or find out: accounts, money, licences, publishing, or a contradiction in a milestone.
  - Creative choices (story, names, look, features) and technical choices are the agents' own: record them.
- Follow-up work: `pm task add`.
- Cite milestones, plans, tasks, decisions by id and subject, in backticks: `decision 3: Unit of length`.

## Outside pm workers
- Skip this section if `PM_TASK` is set (you are a pm worker).
- Milestones are the developer's; edit them with pm when we answer decisions: `pm decision list`, `pm milestone set <id> --body - <<'EOF'`, then `pm decision answer <id> …`.
- Editing a milestone makes pm queue a revision of its plans and tasks; editing the milestone is usually enough.

## luv
- `../luv` (sibling of this repo): a read-only reference checkout of github.com/mbrock/luv. Read its `AGENTS.md`, `README.md`, `wiki/` and code; never change or commit there.
- This repo builds against luv pinned as a flake input, not against `../luv`.
- Something luv lacks: build it here, in this repo; list what would better live in luv in `docs/luv-wishlist.md`.

## Rules
- Push directly to `main`. No feature branches or pull requests.
- Before every commit: the project's build and tests pass (commands below, once they exist).
- Use comments sparingly and keep them short.
- Style of `.md` files: bullet points, concise. Exception: an intro paragraph in full sentences at the top of the document and under each top-level heading.
- Stop background processes by PID (`kill $!`), never `pkill -f`: the pattern can match your own `claude` process.
- Close every game window you open; a worker leaves no process running.
- Unfinished game parts: `PLACEHOLDER(<task id>)` in code and, where visible, on screen.

## Commands
- Dev shell: `direnv allow` or `nix develop`; it provides `pm`.
- Build, run, test, screenshot: to be filled in by `milestone 1`.

## Layout
- `docs/`: scope, design, story, architecture; the source of truth for features after `milestone 1`.
