# Packages the official vkd3d-proton release tarball's prebuilt Windows
# DLLs for use by Decanter's Bottle.Logic.Direct3dWrappers wine-prefix
# installer.
#
# nixpkgs' own "vkd3d-proton" package only ships native Unix libraries meant
# for building Wine itself with vkd3d support baked in -- unlike "dxvk",
# whose nixpkgs package already provides real x64/x32 Windows DLLs, it has
# nothing that can be symlinked into an existing wine prefix's
# system32/syswow64. This derivation fills that gap by unpacking the
# upstream release tarball directly and keeping only its x86/x64 DLL
# directories (the tarball's own "setup_vkd3d_proton.sh" is not used;
# Decanter installs the DLLs itself).
{ lib, stdenvNoCC, fetchurl, zstd }:

stdenvNoCC.mkDerivation rec {
  pname = "vkd3d-proton-decanter";
  version = "3.0.1";

  src = fetchurl {
    url = "https://github.com/HansKristian-Work/vkd3d-proton/releases/download/v${version}/vkd3d-proton-${version}.tar.zst";
    hash = "sha256-PPIxVSKvXkNgXvbTxB2tkThwQL+XGZk08/erdsqqLww=";
  };

  nativeBuildInputs = [ zstd ];

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r x86 $out/x86
    cp -r x64 $out/x64
    runHook postInstall
  '';

  meta = {
    description = "Prebuilt vkd3d-proton Windows DLLs (x86/x64), repackaged from the upstream release tarball";
    homepage = "https://github.com/HansKristian-Work/vkd3d-proton";
    license = lib.licenses.lgpl21Plus;
    platforms = lib.platforms.linux;
  };
}
