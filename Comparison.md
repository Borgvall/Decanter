# Decanter vs. Bottles vs. Faugus Launcher

> **⚠️ Point-in-time snapshot, not a living document.** This comparison
> reflects Bottles and Faugus Launcher as researched in July 2026, based on
> their public repositories and documentation. Both projects evolve
> independently of Decanter - feature lists, CI setups, and test coverage can
> change at any time. Treat this as a reference for orientation, not a
> guarantee of current upstream behavior; verify against the linked
> repositories before relying on a specific claim.

## Summary

| | **Decanter** | **Bottles** | **Faugus Launcher** |
|---|---|---|---|
| **Stack** | Haskell, GTK4/Libadwaita, built with Nix Flakes | Python/GTK4, primarily distributed as Flatpak | Python (~99%), GTK4 via PyGObject, Meson build |
| **Core model** | One bottle = one BTRFS subvolume (with fallback), 1:1 isolation per bottle | One bottle = a "container" shared by **multiple** applications, explicitly *not* one prefix per app | No "bottle" concept - each game implicitly gets a prefix under `~/Faugus/`, managed via UMU-Launcher |
| **Scope** | Full prefix manager: environments, runner switching, snapshots, app-menu integration, CLI | Full prefix manager with dependency manager, templates, ProtonDB integration, task manager | Deliberately minimal **game launcher**, no container management - focused on "add exe → play" |
| **Snapshots/versioning** | Native BTRFS subvolume snapshots, fast save/restore of bottle state | Own versioning system to restore "bottle state", implemented application-side rather than via BTRFS | None |
| **Runner management** | System Wine or Proton, auto-detected from `~/.steam/root/compatibilitytools.d` | Own runner management incl. its own "Bottles Runtime", many Wine forks selectable | Proton Manager to download/remove runners (GE-Proton, Proton-EM), sourced from Steam's `compatibilitytools.d` |
| **Reproducible builds/deploys** | Nix Flake - deterministic build/install, no Docker/Flatpak detour | Primarily Flatpak sandboxing | Flathub/distro packages |
| **Automated backend tests** | Unit/integration test suite for backend logic, enforced by a pre-commit hook (see [`AGENTS.md`](AGENTS.md)) | None found: no `tests/` directory anywhere in the repo; CI (`build_pkgs_release.yml`, `build_pkgs_unstable.yml`, `test_appimage_build.yml`, `build_test.yml`) only builds packages or runs a "Test meson build" compile smoke test, no `pytest`/`unittest` | None found: no test files, and `.github/` only contains `ISSUE_TEMPLATE` - no CI workflows at all |
| **Extra tooling** | winecfg/regedit/uninstaller/winetricks/`wineserver -k`, Direct3D wrapper switch (DXVK/vkd3d-proton for System Wine), app-menu integration, shell completion | Dependency-installer database, DXVK/esync/fsync optimizations, ProtonDB integration, built-in Wine task manager | MangoHud, Feral GameMode, Winetricks, Cheat Engine/mod add-ons, lossless scaling/frame generation, Steam shortcut creation |
| **Target audience** | Users who want Nix reproducibility + fast BTRFS snapshots, more general Windows software than pure gaming | Broad user base, "one app for everything", strong focus on community dependency database | People who already know what they want to install and want a minimal, fast game launch without container overhead |

## Where Decanter positions itself

Bottles is the broadest/most feature-rich approach (multiple apps per prefix,
dependency database, ProtonDB); Faugus is deliberately minimal (a launcher,
no container concept at all). Decanter sits in between on scope, but has
three things neither of the other two offers: **BTRFS-native snapshots** as
a core architectural choice (not bolted on), **Nix-flake reproducibility**
instead of Flatpak/distro packaging, and an **enforced backend test suite**
that backs up that reproducibility with verified correctness rather than
just deterministic builds.

## Sources

- [Why Bottles? | Bottles docs](https://docs.usebottles.com/faq/why-bottles)
- [GitHub - winegame/bottles](https://github.com/winegame/bottles)
- [Bottles - ArchWiki](https://wiki.archlinux.org/title/Bottles)
- [GitHub - Faugus/faugus-launcher](https://github.com/Faugus/faugus-launcher)
- [Faugus Launcher for Linux | App For That](https://appforthat.net/linux/io.github.Faugus.faugus-launcher/)
- [Faugus Launcher is a simple and lightweight app | LinuxLinks](https://www.linuxlinks.com/faugus-launcher-simple-lightweight-app-games/)
