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

        runtimeDeps = with pkgs; [
          wineWowPackages.stable
          winetricks
          xdg-utils
          btrfs-progs
        ];

        # 1. Das "rohe" Paket, wie es aus der Cabal-Datei gelesen wird.
        # Wir definieren es hier separat, damit wir es sowohl für das fertige Paket
        # als auch für die Entwicklungsumgebung nutzen können.
        rawDecanterPkg = pkgs.haskellPackages.callCabal2nix "decanter" ./. {};

      in
      {
        # 2. Das fertige Paket für `nix build` / `nix run`
        packages.default = rawDecanterPkg.overrideAttrs (oldAttrs: {
          doCheck = false;

          nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
            pkgs.pkg-config
            pkgs.wrapGAppsHook4
            pkgs.gobject-introspection
            pkgs.copyDesktopItems
          ];

          buildInputs = (oldAttrs.buildInputs or []) ++ [
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

        # 3. Die Entwicklungsumgebung (nix develop)
        # Wir nutzen `shellFor`. Das registriert alle Abhängigkeiten von `rawDecanterPkg`
        # (also gi-gtk4, text, typed-process etc.) in der GHC-Datenbank der Shell.
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ rawDecanterPkg ];
          
          # Optional: Baut auch die Dokumentation für alle Deps (nice to have)
          withHoogle = true; 

          # Build-Tools (laufen auf dem Host)
          nativeBuildInputs = with pkgs; [
            cabal-install
            haskell-language-server
            hlint
            pkg-config # Wichtig! Damit Cabal die C-Libs (GTK4) findet
          ];

          # System-Bibliotheken (gegen die gelinkt wird)
          # Da callCabal2nix manchmal C-Deps nicht automatisch in die Shell propagiert,
          # fügen wir sie hier sicherheitshalber hinzu.
          buildInputs = with pkgs; [
            gtk4
            libadwaita
            adwaita-icon-theme
            
            # Runtime-Tools (damit du sie beim Testen direkt hast)
            wineWowPackages.stable
            winetricks
          ];
        };
      }
    );
}
