{ pkgs ? import <nixpkgs> {}
, compiler ? null
}:
pkgs.stdenv.mkDerivation {
  name = "hgdbmi-shell";
  buildInputs = [
    (import ./default.nix { inherit pkgs; inherit compiler; }).env.buildInputs
    pkgs.gcc
    pkgs.gdb
    pkgs.util-linux
  ];
}
