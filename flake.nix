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
        packages.default = decanterPkg.overrideAttrs (oldAttrs: {
          # Deaktiviere Tests während des Builds.
          # Grund: Die Tests benötigen eine laufende Wine-Instanz und BTRFS,
          # was in der Nix-Build-Sandbox nicht oder nur schwer möglich ist.
          doCheck = false;

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
          inputsFrom = [ self.packages.${system}.default ];
          
          packages = with pkgs; [
            cabal-install
            haskell-language-server
            hlint
            # Runtime-Tools für manuelle Tests in der Shell ('cabal test')
            wineWowPackages.stable
            winetricks
          ];
        };
      }
    );
}
