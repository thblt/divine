with import <nixpkgs> {};

{ nixpkgs ? import <nixpkgs> {} }:

  stdenv.mkDerivation rec {
    name = "divine-env";

    buildInputs = with pkgs; [
      emacs
      gnumake
      sass
  ];
}
