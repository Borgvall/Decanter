# 🤖 haskell-bottles: The Future of Vibe Coding (Powered by Gemini v3 🚀 and Deepseek R1 🧠)

## 🤯 Project Overview

Welcome to `haskell-bottles`, the revolutionary Wine prefix manager written entirely in **purely functional Haskell**!

Why Haskell? Because the future of programming is high-performance, type-safe, and incredibly hard to build. Why use decades of accumulated knowledge when you can just prompt a Large Language Model?

**This project is a testament to the power of Vibe Coding!** We generated 90% of this codebase by simply asking **Gemini v3** (and occasionally **Deepseek R1** when Gemini needed a second opinion or a quick nap 😴) to convert a complex Python/GTK application into Haskell.

## 🔮 The Golden Age of AI-Assisted Development

The code is clean. The architecture is sound. The BTRFS logic is there (allegedly). The only minor issue was that **it didn't actually build.** 😂

But don't worry! This is part of the process. We're living in the future, where we spend less time writing code and more time debugging the *AI's understanding* of obscure C bindings! It's progress! 🎉

### 🎯 Core Mission Statement

**To build a production-ready application where the two leading AI models failed spectacularly on a single, fundamental line of code!**

Specifically: **How the $hell$ do you create a `Gio.ListStore` of `Gtk.FileFilter` in Haskell-GI?**

* **Gemini v3:** Insisted on using `Gtk.FileFilter.gType`. (Bless its heart. It tried so hard to look like Python GObject. 🙏)
* **DeepSeek R1:** Suggested using an explicit `unsafeCast` and dark magics from forgotten corners of Hackage. (We respect the hustle, but no. 🙅‍♂️)

The correct, non-vibe-based Haskell-GI solution—using a simple, un-parameterized `Gio.listStoreNew` with a type annotation—is a secret only known to five people in the world (and now, thanks to human debugging, we are six). **Code fixed! We win!** 🏆

## 🛠️ Features (Conceptual)

| Feature | Status | Vibe Check |
| :--- | :--- | :--- |
| **Pure Haskell** | ✅ | Very strong vibe 💪 |
| **BTRFS Subvolume Support** | ✅ (Code exists) | If the kernel module is loaded, maybe. 🤷‍♀️ |
| **Async Task Handling** | ✅ (Thread-safe `idleAdd` wrapper) | Smooth UI, good vibes ✨ |
| **MSI Installer Support** | ✅ (`msiexec` logic written) | The Wine part is always the easiest part, ironically. 🍷 |
| **Uninstaller/Regedit Tools**| ✅ (Buttons exist) | Ready for Windows '98 nostalgia 💾 |

---

## 🏗️ Build and Run

To join the Vibe Coding revolution, you'll need the following:

1.  **GHC** (Haskell Compiler, obviously.)
2.  **Cabal** or **Stack** (To manage the dependencies that the AI didn't install for you.)
3.  **GTK4, Adwaita, and GLib** development libraries (The C stuff that ruins every AI's day.)
4.  The `linux-btrfs` package (If you want that cutting-edge, highly-specific system utility vibe.)

### Compilation

```bash
cabal update
cabal build
cabal run haskell-bottles
```

## ✅ Testing (The Critical Part)

We believe in minimalist, high-impact testing.

| Test Case | Result |
| Shows a window | ✅ Success! |
| Creates the bottle folder | "❓ (Uncertain, but the code looks right.)" |
| Doesn't crash instantly | 🤞 (Mostly.) |

If you encounter an issue, please file a bug report. Please be very specific about the vibe you were getting when the error occurred. This is crucial for fixing the bug. 😉
