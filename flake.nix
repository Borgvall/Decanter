{
  description = "Decanter - A modern Wine prefix manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # --- KONFIGURATION ---
        # Wähle hier die gewünschte Wine-Version.
        # Optionen in nixpkgs:
        # - pkgs.wineWowPackages.stable      (Stabil, älter)
        # - pkgs.wineWowPackages.staging     (Aktueller, Patchset für Gaming/Kompatibilität)
        # - pkgs.wineWowPackages.unstable    (Bleeding Edge)
        # - pkgs.wineWowPackages.wayland     (Experimentell)
        
        selectedWine = pkgs.wineWowPackages.staging; 
        # Ich empfehle 'staging' für Decanter, da es oft bessere Kompatibilität 
        # für moderne Windows-Apps hat als 'stable'.

        runtimeDeps = with pkgs; [
          selectedWine           # Nutzt die oben gewählte Version
          winetricks
          xdg-utils
          btrfs-progs
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
            
            # Auch in der Dev-Shell nutzen wir jetzt exakt die gewählte Wine-Version
            selectedWine
            winetricks
          ];
        };
      }
    );
}
