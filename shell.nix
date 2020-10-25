{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = with pkgs; [
      elmPackages.elm
      elmPackages.elm-format
      elmPackages.elm-test
      elm2nix
      nodejs
      nodePackages.parcel-bundler
    ];
}
