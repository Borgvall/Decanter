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

      in
      {
        packages.default = rawDecanterPkg.overrideAttrs (oldAttrs: {
          doCheck = true;

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
