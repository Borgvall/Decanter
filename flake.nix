{
  description = "Decanter - A modern Wine prefix manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # --- KONFIGURATION ---
        # Wähle hier die gewünschte Wine-Version.
        # Optionen in nixpkgs:
        # - pkgs.wineWow64Packages.stable      (Stabil, älter)
        # - pkgs.wineWow64Packages.staging     (Aktueller, Patchset für Gaming/Kompatibilität)
        # - pkgs.wineWow64Packages.unstable    (Bleeding Edge)
        # - pkgs.wineWow64Packages.wayland     (Experimentell)
        selectedWine = pkgs.wineWow64Packages.staging; 

        runtimeDeps = with pkgs; [
          selectedWine
          winetricks
          xdg-utils
          btrfs-progs
          umu-launcher
        ];

        rawDecanterPkg = pkgs.haskellPackages.callCabal2nix "decanter" ./. {};

        # nixpkgs' own "vkd3d-proton" package has no usable Windows DLLs
        # (see vkd3dproton-decanter.nix for the full rationale), so we
        # repackage the upstream release tarball ourselves.
        vkd3dProtonDecanter = pkgs.callPackage ./vkd3dproton-decanter.nix {};

      in
      {
        packages.default = rawDecanterPkg.overrideAttrs (oldAttrs: {
          doCheck = true;

          # Nix store paths of the "dxvk" and vkd3d-proton-decanter
          # packages, used by Bottle.Logic.Direct3dWrappers to symlink
          # their DLLs into a wine prefix. Set as plain derivation
          # attributes so they're already environment variables during
          # checkPhase (where the test suite runs); preFixup below
          # additionally exposes them to the installed binary at runtime.
          #
          # "pkgs.dxvk" is a multi-output derivation whose *default* output
          # is just a "setup_dxvk.sh" wrapper script -- no actual DLLs. The
          # compiled Windows DLLs (x32/x64 directories) live in its separate
          # "bin" output, "pkgs.dxvk.bin".
          DECANTER_DXVK_PATH = "${pkgs.dxvk.bin}";
          DECANTER_VKD3D_PROTON_PATH = "${vkd3dProtonDecanter}";

          nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
            pkgs.pkg-config
            pkgs.wrapGAppsHook4
            pkgs.gobject-introspection
            pkgs.copyDesktopItems
            pkgs.procps # pgrep, used by the System Wine kill test
          ];

          buildInputs = (oldAttrs.buildInputs or []) ++ [
            selectedWine
            pkgs.gtk4
            pkgs.libadwaita
            pkgs.adwaita-icon-theme
          ];

          preFixup = (oldAttrs.preFixup or "") + ''
            gappsWrapperArgs+=(--prefix PATH : "${pkgs.lib.makeBinPath runtimeDeps}")
            gappsWrapperArgs+=(--set DECANTER_DXVK_PATH "${pkgs.dxvk.bin}")
            gappsWrapperArgs+=(--set DECANTER_VKD3D_PROTON_PATH "${vkd3dProtonDecanter}")
          '';

          postInstall = (oldAttrs.postInstall or "") + ''
            mkdir -p $out/share/applications
            cp data/com.github.borgvall.decanter.desktop $out/share/applications/
            
            mkdir -p $out/share/icons/hicolor/scalable/apps
            cp data/com.github.borgvall.decanter.svg $out/share/icons/hicolor/scalable/apps/
          '';
        });

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ rawDecanterPkg ];
          withHoogle = true;

          # So "cabal test" can find DXVK/vkd3d-proton the same way the
          # packaged build does.
          shellHook = ''
            export DECANTER_DXVK_PATH="${pkgs.dxvk.bin}"
            export DECANTER_VKD3D_PROTON_PATH="${vkd3dProtonDecanter}"
          '';

          nativeBuildInputs = with pkgs; [
            cabal-install
            haskell-language-server
            hlint
            pkg-config
          ];

          buildInputs = with pkgs; [
            gtk4
            libadwaita
            adwaita-icon-theme
            
            selectedWine
            winetricks
            umu-launcher
          ];
        };
      }
    );
}
