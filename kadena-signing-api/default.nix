{ pactRef ? "ace18a2489bf86595af09316bd1c09794c326b77"
, pactSha ? "13cg8nz8lc839xnfp31yj8ihra5qpgfz9g4lzqmgcfk4kmxcvlxi"
}:

let

pactSrc = builtins.fetchTarball {
  url = "https://github.com/kadena-io/pact/archive/${pactRef}.tar.gz";
  sha256 = pactSha;
};
pact = import "${pactSrc}/project.nix" {};

in pact.rp.project ({ pkgs, hackGet, ... }:
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
    overrides = import ./overrides.nix pactSrc pkgs hackGet;

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
