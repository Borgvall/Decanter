# Decanter

**Decanter** is a modern Wine prefix manager written in **Haskell**, utilizing **GTK4** and **Libadwaita** to provide a native Linux experience.

> **⚠️ Experimental Project**
> This project is a proof-of-concept created to test the coding capabilities of **Gemini V3** (AI). The code, structure, and logic were largely generated as part of this experiment.
>
> **Analyze the Process:**
> If you look at the **Git commit history**, you will find the original **German prompts and AI responses** in the commit messages. This history serves as a transparent documentation of the development workflow. It allows you to trace exactly:
> * Where the AI produced flawless, working code immediately.
> * Where manual intervention was necessary to fix bugs, resolve compiler errors, or adjust the logic.

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

## 🛠 Tech Stack

* **Language:** Haskell
* **UI Toolkit:** GTK4 + Libadwaita (via `haskell-gi`)
* **Build System:** Cabal
* **Dependencies:** `typed-process`, `directory`, `btrfs`, `text`

## 📦 Prerequisites

To build and run Decanter, you need the following system dependencies:

1.  **GHC & Cabal:** (Haskell compiler and package manager)
2.  **Wine:** Must be installed on your system.
3.  **GTK4 & Libadwaita Development Headers:**
    * **Debian/Ubuntu:** `libgtk-4-dev libadwaita-1-dev`
    * **Fedora:** `gtk4-devel libadwaita-devel`
    * **Arch:** `gtk4 libadwaita`

**Note:** GTK 4.20 or newer is needed, because the return type of `Gtk.fileDialogOpenFinish` has changed.

## 🔨 Build & Run

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

## 📄 License

This project is licensed under the GPL-3.0.
