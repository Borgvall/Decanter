# Project rules for coding agents (Claude Code & co.)

This project is a reproducible Nix flake application (Haskell/GTK4). The
rules below are **also** enforced technically via hooks (see
`.claude/settings.json` and `.githooks/pre-commit`) - but please follow
them proactively instead of relying on the hook as a safety net.

**Before proposing new features or larger changes**, check
[`Readme.md`](Readme.md)'s Vision and Non-Goals sections. Decanter
deliberately stays narrower than Bottles, and some things (a built-in
application launcher, Proton version management, curated per-app install
recipes) are intentionally out of scope - see Non-Goals there for the
reasoning.

## 1. `cabal build` must succeed before every commit

Before running `git commit`, make sure that

```bash
cabal build --enable-tests all
```

completes without errors. A `PreToolUse` hook intercepts `git commit`
calls and automatically aborts the commit if the build fails - don't
commit "on faith" anyway; fix build errors first.

## 2. Newly exported functions in any "Logic" module need tests

This rule applies to **every module whose name or path contains "Logic"**
- not just `Bottle/Logic.hs`, but also e.g. `Logic/Translation.hs`, or any
future module like `Foo/LogicBar.hs`.

If you change such a module and add a new function to its export list
(i.e. `funcName` appears in the list between `module Some.Logic ( ... )
where`), add a matching test case to its spec module in the same commit.
The spec module follows the standard `hspec-discover` naming convention:

| Module               | Spec module                    |
| --------------------- | ------------------------------- |
| `Bottle/Logic.hs`      | `test/Bottle/LogicSpec.hs`       |
| `Logic/Translation.hs` | `test/Logic/TranslationSpec.hs`  |

Example test case:

```haskell
describe "myNewFunction" $ do
  it "behaves as expected" $ do
    myNewFunction input `shouldBe` expectedResult
```

**Non-exported (internal) helper functions never trigger this check** -
only what actually becomes part of a Logic module's public API needs its
own test case.

## 3. Comments and documentation are written in English

All code comments, docstrings, commit messages for tooling changes, and
project documentation (like this file) are written in English, even
though the project itself may be discussed with maintainers in German
elsewhere. This keeps the codebase consistent and accessible to the
widest possible set of contributors and tools.

## 4. GUI changes are tested by the user, not by simulated clicks

For changes to any `Gui/*` module, do not drive the running app
yourself with simulated mouse/keyboard input (e.g. `wlrctl`, `ydotool`,
`xdotool`) and screenshots (e.g. `grim`) to verify the change. The
project's desktop session is the user's real, live session - hijacking
its cursor to click through the app is disruptive and can land clicks
in unrelated windows.

Instead:

1. Build and run the automated test suite as usual
   (`cabal build --enable-tests all`, `cabal test`).
2. Launch the app with `cabal run` so it's ready on screen.
3. Write a short, concrete checklist of what to click and what to
   expect (e.g. "click the 'Change Runner' button - does a popover
   open directly under it, listing all runners with the current one
   checked?").
4. Stop and wait for the user's own feedback instead of clicking
   through the checklist yourself.

This doesn't apply to non-interactive checks - building, the automated
test suite, and `nix build` remain fine to run without asking.

## 5. Splitting a module into submodules

When a module grows too large and gets split into cohesive submodules
(as done for `Bottle.Logic` -> `Bottle.Logic.Process`/`Snapshots`/
`Direct3dWrappers`/`ApplicationMenu`/`Runner`/`Programs`, and for
`Gui.BottleView` -> `Gui.ProgramListView`):

- Follow the existing per-directory naming convention. `Bottle.Logic.*`
  submodules use the nested dotted name (e.g. `Bottle.Logic.
  ApplicationMenu`). `Gui.*` submodules stay flat instead (e.g.
  `Gui.ProgramListView`, not `Gui.BottleView.ProgramList`), matching
  the sibling files already there (`Gui.BottleSnapshotsView`,
  `Gui.OverviewView`, ...).
- Don't leave the split-off functions re-exported through the original
  module "for convenience". Have every caller (Cli, Main, Gui.*, the
  test suite) import each submodule directly for what it actually
  uses; the original module itself should only import (without
  re-exporting) whatever it still genuinely calls internally. A pure
  passthrough re-export just hides the real dependency graph and makes
  GHC rebuild every caller whenever any submodule changes, even ones
  unrelated to what that caller uses.
- Watch out for a trap in the test-coverage hook from rule 2: it diffs
  a file's export list against its content at `HEAD`. For a
  brand-new file, that means *every* exported function counts as
  "newly exported" - even ones that only moved there from elsewhere
  and already had tests. Don't satisfy this mechanically (e.g. by just
  mentioning the name in a comment); move the existing test cases into
  a new spec module alongside the split-off code, and only write
  genuinely new tests for functions that were newly made public in the
  process (e.g. a helper that used to be private).
- Prefer one commit per extracted submodule so the history stays
  reviewable. Dropping the original module's re-exports in favor of
  direct imports at every call site is naturally its own follow-up
  commit, once the split itself is confirmed working.

## 6. Removing a feature: keep reading old persisted data

Decanter persists per-bottle state to disk across versions (e.g.
`decanter.cfg`). When a feature that affected that format gets
removed (as happened when 32-bit prefix support was dropped, changing
`decanter.cfg` from a `(RunnerType, Arch)` tuple to a bare
`RunnerType`), keep read-compatibility for the old format even though
writing it is gone - don't let existing installs silently fall through
to a generic fallback that discards previously-recorded settings (e.g.
which runner a bottle used).

A private, non-exported stand-in type/parser for the old shape (see
`Bottle.Logic`'s `LegacyArch`) is enough; it doesn't need to be part of
the public API, just present so `reads`/parsing can still recognize
and salvage the old data.

## 7. Use agent-lsp for code intelligence, not manual search or shell archaeology

This project's toolset includes agent-lsp, which indexes both this
repository's own code and its full Haskell dependency closure (gi-gtk4,
gi-adwaita, ...). Prefer its tools over `Grep`/`Read`-and-scan or shell
commands for any code-intelligence question:

- Finding a symbol, its definition, or a file's exports: `find_symbol` /
  `list_symbols` instead of grepping for a name.
- Finding usages/callers of a symbol, or the blast radius of a change
  before editing an exported function: `find_references` /
  `find_callers` / `blast_radius` instead of grepping for the name
  across files.
- Third-party package details - e.g. "does `Gtk.Popover` have a `closed`
  signal?", "what's the exact type of `listBoxAppend`?" - `inspect_symbol`
  / `get_symbol_documentation` instead of grepping cabal store tarballs
  or hackage source dumps.

For a single, targeted lookup like any of the above, call the tool
directly instead of spawning a subagent to do it on your behalf - a
subagent just makes the identical tool call one round trip later, with
no benefit. Reserve subagents for genuinely multi-step research
(correlating several files, walking a large caller graph, or anything
whose verbose intermediate output is worth keeping out of the main
conversation).

## 8. Give a function a pure signature unless it actually needs IO

When writing or reviewing a function - especially one just extracted out
of an existing `do` block - check whether it actually performs I/O
(filesystem, process, environment, network, mutable state) rather than
defaulting to whatever monad the surrounding code happens to run in. A
function that only computes a result from its arguments (a pattern
match, a lookup, a formatting step, ...) should have a pure type
signature, not an `IO` one it never needed.

This is easy to get wrong precisely because it doesn't show up as a
build failure - an unnecessarily-`IO` function still type-checks and
its tests still pass either way, so nothing forces a second look. One
recurring case: a helper that needs to abort on an "unreachable" branch
doesn't need `IO` just for that - `Control.Exception.throw` throws
lazily (once its result is forced) and works fine from a pure function,
so `throwIO` alone shouldn't be the reason to keep something in `IO`
(see `Bottle.Logic.createBottleLogic`'s `bootCmd`/`bootArgs` tuple and
`Bottle.Logic.Process.getProtonEnv`, two functions that only
pattern-match on `RunnerType` and stayed/became pure this way).

## 9. New integration tests that create a real bottle: use `withTestBottle`

Any test that calls `createBottleLogic` on a real bottle must guarantee
`deleteBottleLogic` still runs even if an assertion in between fails.
This matters more than it looks: some spec modules
(`Bottle.Logic.ProcessSpec`, `Bottle.Logic.Direct3dWrappersSpec`)
deliberately reuse a `dist-newstyle/decanter-test-xdg-data-home`
directory across test runs (kept around only to avoid re-downloading
umu-run's runtime every time), so a skipped cleanup call there leaks a
stale bottle across `cabal test` invocations, not just within a single
run.

Use `test/Bottle/Logic/TestSupport.hs`'s `withTestBottle :: Bottle ->
(Bottle -> IO ()) -> IO ()` instead of a manual
`createBottleLogic`/`...`/`deleteBottleLogic` pairing - it runs the test
body under `finally`, so the bottle is always deleted, even on a
failing expectation or a thrown exception.

## Commit messages

The author of an AI commit shall be the used LLM name and version e.g. Opus
4.5. The author's email shall be `no@reply.org`.

The first line of the commit message shall summarize the changes in 80
characters. The first word should be one of "Feat", "Fix", "Refactor", "Tests",
"Docs" or "Chore" followed by a ':', that indicates the intended scope of the
commit. A good example line is "Refactor: split module Foo into submodules".

The commit message's body shall summarize the requirements from the prompts and
supplied extra informations at the beginning. The following paragraphs shall
summarize the chosen design decisions.
