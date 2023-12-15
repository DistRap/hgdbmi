pkgs: hself: hsuper:
let
  lib = pkgs.lib;
  haskellLib = pkgs.haskell.lib;
in
{
  hgdbmi =
    lib.pipe
      (hself.callCabal2nix "hdbmi" ./. {})
      [
        haskellLib.compose.buildFromSdist
        (pkg: pkg.overrideAttrs (attrs: {
          buildInputs = attrs.buildInputs ++ [
            # io testsuite
            pkgs.gcc
            pkgs.gdb
            # due to setsid
            pkgs.util-linux
          ];
        }))
      ];
}
