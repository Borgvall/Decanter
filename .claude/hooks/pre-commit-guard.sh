#!/usr/bin/env bash
#
# PreToolUse hook for Claude Code (see .claude/settings.json).
#
# Runs before EVERY Bash tool call. Only acts when the command about to be
# executed contains 'git commit', in which case it runs
# .githooks/pre-commit (a Haskell script). If that check fails - or the
# script is missing / not executable - the 'git commit' call is blocked
# via exit code 2 (see the Claude Code hooks reference: PreToolUse + exit
# 2 = the tool call is prevented, stderr is returned to Claude as an error
# message).
#
# The actual check logic (cabal build + test coverage for newly exported
# functions in any "Logic" module) intentionally lives in
# .githooks/pre-commit, so it is identical whether a human or Claude Code
# is committing.

set -uo pipefail

INPUT="$(cat)"

# Only act on 'git commit' invocations.
# (Plain substring check on the raw JSON input, so no extra JSON tool like
# 'jq' is required.)
if ! grep -q "git commit" <<< "$INPUT"; then
  exit 0
fi

REPO_ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null)}"

if [ -z "$REPO_ROOT" ]; then
  echo "Could not determine the repository root. Blocking the commit to be safe." >&2
  exit 2
fi

HOOK_SCRIPT="$REPO_ROOT/.githooks/pre-commit"

# If the hook is missing or not executable, abort instead of letting the
# commit through unchecked - a false alarm is preferable to a silently
# skipped check.
if [ ! -x "$HOOK_SCRIPT" ]; then
  echo "" >&2
  echo "❌ $HOOK_SCRIPT was not found or is not executable." >&2
  echo "   The 'git commit' command was blocked to be safe." >&2
  echo "   Please make sure .githooks/pre-commit is present in the repository" >&2
  echo "   with its executable bit set (it ships that way when applied from" >&2
  echo "   a Git patch)." >&2
  exit 2
fi

if "$HOOK_SCRIPT"; then
  exit 0
else
  echo "" >&2
  echo "The pre-commit check ('.githooks/pre-commit') failed." >&2
  echo "The 'git commit' command was blocked. Please fix the issues reported" >&2
  echo "above (cabal build and/or missing test cases for newly exported" >&2
  echo "functions in a Logic module) and try again." >&2
  exit 2
fi
