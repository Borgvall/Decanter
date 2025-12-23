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
        runtimeDeps = with pkgs; [
          wineWowPackages.stable
          winetricks
          xdg-utils
          btrfs-progs
        ];

        # Definiere das Basis-Haskell-Paket
        decanterPkg = pkgs.haskellPackages.callCabal2nix "decanter" ./. {};

      in
      {
        # Wir nutzen overrideAttrs statt overrideCabal, um den Builder-Fehler zu umgehen.
        # Dies modifiziert direkt die Umgebung des Derivats (mkDerivation), was robuster ist.
        packages.default = decanterPkg.overrideAttrs (oldAttrs: {
          # Build-Tools für die Umgebung (pkg-config findet libs, Hooks wrappen die App)
          nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
            pkgs.pkg-config
            pkgs.wrapGAppsHook4
            pkgs.gobject-introspection
            pkgs.copyDesktopItems
          ];

          # C-Bibliotheken, die via pkg-config gefunden werden müssen
          buildInputs = (oldAttrs.buildInputs or []) ++ [
            pkgs.gtk4
            pkgs.libadwaita
            pkgs.adwaita-icon-theme
          ];

          # Wrapper-Argumente setzen (Wine & Co. in den Pfad aufnehmen)
          preFixup = (oldAttrs.preFixup or "") + ''
            gappsWrapperArgs+=(--prefix PATH : "${pkgs.lib.makeBinPath runtimeDeps}")
          '';

          # Desktop-File und Icon installieren
          postInstall = (oldAttrs.postInstall or "") + ''
            mkdir -p $out/share/applications
            cp data/com.github.borgvall.decanter.desktop $out/share/applications/
            
            mkdir -p $out/share/icons/hicolor/scalable/apps
            cp data/com.github.borgvall.decanter.svg $out/share/icons/hicolor/scalable/apps/
          '';
        });

        # Entwicklungsumgebung
        devShells.default = pkgs.mkShell {
          # Wir erben alle Inputs vom fertigen Paket, damit alles da ist
          inputsFrom = [ self.packages.${system}.default ];
          
          packages = with pkgs; [
            cabal-install
            haskell-language-server
            hlint
            # Runtime-Tools auch in der Shell verfügbar machen
            wineWowPackages.stable
            winetricks
          ];
        };
      }
    );
}
