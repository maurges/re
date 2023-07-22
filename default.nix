{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
    ron-hs = pkgs.fetchFromGitHub {
      owner = "d86leader";
      repo = "ron-hs";
      rev = "e01e6acd0970c3e236450e10aeff5f22e135ed42";
      hash = "sha256-U3Crc2ur9MR+wbt5NkFpOEa+4kJ0+0THdQNaTB0kNBk=";
    };
  };
}
