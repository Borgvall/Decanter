{
  description = "Decanter - A modern Wine prefix manager";

  # Wir nutzen NixOS 25.11 als stabile Basis für GTK4/Libadwaita Versionen.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Laufzeit-Programme, die Decanter aufruft.
        # Diese werden in den PATH des Wrappers aufgenommen.
        runtimeDeps = with pkgs; [
          wineWowPackages.stable # Wine (32/64 bit support)
          winetricks
          xdg-utils              # Für xdg-open
          btrfs-progs            # Für btrfs CLI calls (Fallback/Checks)
        ];

        # Definiere das Haskell-Paket
        decanterPkg = pkgs.haskellPackages.callCabal2nix "decanter" ./. {};

      in
      {
        packages.default = pkgs.haskell.lib.overrideCabal decanterPkg (old: {
          # Build-Tools für das Kompilieren und Wrappen
          nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
            pkgs.pkg-config
            pkgs.wrapGAppsHook4      # Wichtig für GTK4/Adwaita Umgebung
            pkgs.gobject-introspection
            pkgs.copyDesktopItems    # Um die .desktop Datei automatisch zu installieren (optional)
          ];

          # C-Bibliotheken, gegen die gelinkt wird
          buildInputs = (old.buildInputs or []) ++ [
            pkgs.gtk4
            pkgs.libadwaita
            pkgs.adwaita-icon-theme  # Icons für die GUI
          ];

          # Hier injizieren wir den PATH für Wine etc. in den GTK-Wrapper
          preFixup = ''
            gappsWrapperArgs+=(--prefix PATH : "${pkgs.lib.makeBinPath runtimeDeps}")
          '';

          # Installiere die .desktop Datei und das Icon manuell oder via Hook
          postInstall = ''
            mkdir -p $out/share/applications
            cp data/com.github.borgvall.decanter.desktop $out/share/applications/
            
            mkdir -p $out/share/icons/hicolor/scalable/apps
            cp data/com.github.borgvall.decanter.svg $out/share/icons/hicolor/scalable/apps/
          '';
        });

        # Entwicklungsumgebung (nix develop)
        devShells.default = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.default ];
          
          packages = with pkgs; [
            cabal-install
            haskell-language-server
            hlint
            # Damit man Wine/Winetricks auch in der Dev-Shell hat
            wineWowPackages.stable
            winetricks
          ];
        };
      }
    );
}
