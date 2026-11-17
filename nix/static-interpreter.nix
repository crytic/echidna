{
  pkgs,
  buildHaskellPackages,
  hostHaskellPackages,
}:

# Use a patched build-time interpreter for the x86_64 musl release. See
# iserv-symbols.c for the upstream source, crash diagnosis, and replacement's
# purpose. Override only this package's interpreter, leaving dependencies cached.
package:
if pkgs.stdenv.hostPlatform.system != "x86_64-linux" then
  package
else
  let
    interpreter = pkgs.haskell.lib.overrideCabal hostHaskellPackages.iserv-proxy (old: {
      enableLibraryProfiling = false;
      enableExecutableProfiling = false;
      # This path is inside iserv-proxy's unpacked source. Its Cabal file already
      # includes it in the x86_64 Linux interpreter's c-sources.
      postPatch = (old.postPatch or "") + ''
        cp ${./iserv-symbols.c} cbits/symbols.x86_64-musl.c
      '';
    });
    wrapper = pkgs.writeShellScript "echidna-static-iserv" ''
      exec ${buildHaskellPackages.iserv-proxy}/bin/iserv-proxy "$@" --pipe \
        ${interpreter}/bin/iserv-proxy-interpreter tmp --stdio
    '';
  in
  pkgs.haskell.lib.appendConfigureFlags package [
    "--ghc-option=-pgmi"
    "--ghc-option=${wrapper}"
  ]
