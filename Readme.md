# Decanter

**Decanter** is a modern Wine prefix manager written in **Haskell**, utilizing **GTK4** and **Libadwaita** to provide a native Linux experience.

> **⚠️ Experimental Project** This project started as a proof-of-concept to
> test the coding capabilities of AI models. It began with **Gemini**: every
> change was driven by a manual prompt, and both the prompt and the model's
> response were copied verbatim into the commit message.
>
> It has since evolved into **agentic coding** with **Claude Code**, given a
> proper harness to work in - a pre-commit hook that enforces a successful
> build and test coverage for new logic, an `AGENTS.md` describing project
> conventions, and a rule to hand off interactive GUI verification back to a
> human instead of simulating clicks. The commit history reflects this shift:
> earlier commits carry the full German prompt/response pair, later ones are
> shorter and describe requirements and design decisions instead.
>
> **Analyze the process:** the **Git commit history** documents this
> transparently across both eras. It lets you trace exactly:
>
> - Where the AI produced flawless, working code immediately.
> - Where manual intervention was necessary to fix bugs, resolve compiler
>   errors, or adjust the logic.
> - How the workflow itself changed - from manually copy-pasted prompts to
>   an agent operating inside guardrails it has to satisfy on its own.

## 💡 Inspiration

Decanter is heavily inspired by the excellent [**Bottles**](https://usebottles.com/) project. It aims to replicate the core concept of easy-to-manage "bottles" (isolated Wine environments) using a purely functional programming approach with Haskell.

## 🚀 Features

Decanter provides a clean GUI to manage your Windows applications on Linux:

* **Isolated Environments:** Create and manage multiple Wine prefixes ("Bottles") to keep dependencies separate.
* **Architecture Support:** Choose between 32-bit (`win32`) and 64-bit (`win64`) architectures when creating a bottle.
* **Modern UI:** Built with GTK4 and Libadwaita for a seamless GNOME integration.
* **Smart Storage (BTRFS):** Automatically attempts to create bottles as BTRFS subvolumes for better storage management. It gracefully falls back to standard directories if BTRFS is unavailable.
* **Snapshots (BTRFS):** Using BTRFS-subvolume snapshots the state of a battle can be stored and restored fast.
* **Program Detection:** Automatically scans the bottle's Start Menu to find and list installed applications.
* **Drag & Drop:** Simply drag `.exe` or `.msi` files into the bottle view to run or install them.
* **Integrated Tools:** Quick access to essential Wine utilities:
    * `winecfg`
    * `regedit`
    * `uninstaller`
    * `wineserver -k` (Kill all processes)
    * `winetricks` (if installed)
* **Direct3D Wrapper Switch:** For System Wine bottles, toggle between Wine's
  built-in Direct3D implementation, DXVK, and DXVK + vkd3d-proton right from
  the bottle view. Unlike Proton, plain Wine doesn't ship these on its own;
  "DXVK + vkd3d-proton" is recommended for modern (Direct3D 12) games.

## 🛠 Tech Stack

* **Language:** Haskell
* **UI Toolkit:** GTK4 + Libadwaita (via `haskell-gi`)
* **Build System:** Cabal
* **Dependencies:** `typed-process`, `directory`, `btrfs`, `text`

## Build with Nix

Decanter provides a reproducible development environment and build process using Nix Flakes. This is the easiest way to run or develop Decanter without manually installing dependencies like GHC, GTK4, or Wine.

### Run immediately

To run Decanter without installing it (download, build, and run in one go):

```bash
nix run
```

### Install in user profile

To install Decanter in your user profile:

```bash
nix profile add .
```

In order to remove it afterwards:

```bash
nix profile remove Decanter
```

### Development Environment

To start a shell with all dependencies (GHC, Language Server, libraries) pre-configured:

```bash
nix develop
cabal run

```

### Direnv

This project supports [direnv](https://direnv.net/) to automatically load the development environment. Since an `.envrc` file is already included, you can simply run:

```bash
direnv allow

```

Now, whenever you enter the project directory, the Nix environment will be loaded automatically.

## 📦 Prerequisites (Building without Nix)

To build and run Decanter, you need the following system dependencies:

1.  **GHC & Cabal:** (Haskell compiler and package manager)
2.  **Wine:** Must be installed on your system.
3.  **GTK4 & Libadwaita Development Headers:**
    * **Debian/Ubuntu:** `libgtk-4-dev libadwaita-1-dev`
    * **Fedora:** `gtk4-devel libadwaita-devel`
    * **Arch:** `gtk4 libadwaita`

**Note:** GTK 4.20 or newer is needed, because the return type of `Gtk.fileDialogOpenFinish` has changed.

## 🔨 Build & Run (Cabal)

Clone the repository and use Cabal to run the project:

```bash
# Update package list
cabal update

# Build the project
cabal build

# Run the application
cabal run decanter
```

## 📂 Data Location

Decanter stores its bottles in the standard XDG Data directory:

 * `~/.local/share/Decanter/`

## 🏃 Running Tests

```bash
cabal test
```

**Note:** The Proton-based process-killing test downloads a Steam Runtime
(several hundred MB, via `umu-run`) on first run. It is stored under the
build directory (`dist-newstyle/decanter-test-xdg-data-home`), not in your
real `~/.local/share/`, and is deliberately kept there between test runs so
it isn't re-downloaded every time.

## 🧪 Pre-Commit Checks (also enforced for Claude Code)

This repository enforces two rules before every commit, both for human
contributions and for Claude Code:

1. `cabal build` must succeed.
2. Newly **exported** functions in any module whose name or path contains
   "Logic" (e.g. `Bottle/Logic.hs`, `Logic/Translation.hs`) need at least
   one matching test case in the corresponding spec module under `test/`
   (e.g. `test/Bottle/LogicSpec.hs`, `test/Logic/TranslationSpec.hs`).

The check logic is implemented as a Haskell script in
[`.githooks/pre-commit`](.githooks/pre-commit) and is wired up twice:

* as a real Git hook, once `core.hooksPath` points at `.githooks` - enable
  this once per clone with `git config --local include.path ../.gitconfig`
  (the tracked [`.gitconfig`](.gitconfig) sets `core.hooksPath`; the hook
  script already ships with its executable bit set), and
* as a [Claude Code `PreToolUse` hook](.claude/settings.json) that
  intercepts `git commit` calls from Claude Code and blocks them on
  failure - or if the hook script is missing or not executable.

Details and context for coding agents live in [`AGENTS.md`](AGENTS.md).

## 📄 License

This project is licensed under the GPL-3.0.
