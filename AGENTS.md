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

To determine this, the hook compares a Logic module's export list between
`HEAD` and the staged version, and checks, for every newly added exported
function, whether its name appears at least once in the corresponding spec
file. If a test case is missing, the commit is blocked with a list of the
affected functions and modules.

## 3. Comments and documentation are written in English

All code comments, docstrings, commit messages for tooling changes, and
project documentation (like this file) are written in English, even
though the project itself may be discussed with maintainers in German
elsewhere. This keeps the codebase consistent and accessible to the
widest possible set of contributors and tools.

## Hook setup

`core.hooksPath` is not set automatically. Once per clone, enable the
tracked `.gitconfig` (which sets `core.hooksPath` to `.githooks`) with:

```bash
git config --local include.path ../.gitconfig
```

`.githooks/pre-commit` already ships with its executable bit set (Git
preserves file permissions when a patch or commit is applied), so no
additional `chmod` step is required.
