# Project rules for coding agents (Claude Code & co.)

This project is a reproducible Nix flake application (Haskell/GTK4). The
rules below are **also** enforced technically via hooks (see
`.claude/settings.json` and `.githooks/pre-commit`) - but please follow
them proactively instead of relying on the hook as a safety net.

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
