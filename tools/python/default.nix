{ system ? builtins.currentSystem
, pkgs ? import <nixpkgs> {
    inherit system;
  }
}:
let pip2nixSrc = pkgs.fetchFromGitHub {
      owner = "nix-community";
      repo = "pip2nix";
      rev = "0bbd06bcc9cbb372624d75775d938bf028da9f78";
      sha256 = "sha256-YpNCrm3FWXpbv92CHAnkhmjGYTRNun8Iw8q6sVWiaPA=";
    };
    pip2nix = (import "${pip2nixSrc}/release.nix" {}).pip2nix;
    packageOverrides = pkgs.callPackage ./python-packages.nix {};
    python = pkgs.python39.override { inherit packageOverrides; };
in {
  inherit python pip2nix;
  inherit (python.pkgs) schemathesis;
}