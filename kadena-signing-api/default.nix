{ pactRef ? "3b203840de73a33f46a87f7f81eeef86a0d5c97f"
, pactSha ? "0jwld9fx75jfdi25rdyq0i662lykh7hdjqpm1yb0jz294bh27wz2"
}:

let

pactSrc = builtins.fetchTarball {
  url = "https://github.com/kadena-io/pact/archive/${pactRef}.tar.gz";
  sha256 = pactSha;
};
pact = import pactSrc {};

in pact.rp.project ({ pkgs, ... }:
let

gitignoreSrc = pkgs.fetchFromGitHub {
  owner = "hercules-ci";
  repo = "gitignore";
  rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
  sha256 = "0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
};
inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

in {
    name = "kadena-signing-api";
    overrides = import ./overrides.nix pactSrc pkgs;

    packages = {
      kadena-signing-api = gitignoreSource ./.;
    };

    shellToolOverrides = ghc: super: {
      cabal-install = pkgs.haskellPackages.cabal-install;
      ghcid = pkgs.haskellPackages.ghcid;
    };

    shells = {
      ghc = ["kadena-signing-api"];
    };
  })
