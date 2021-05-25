{ pactRef ? "ace18a2489bf86595af09316bd1c09794c326b77"
, pactSha ? "13cg8nz8lc839xnfp31yj8ihra5qpgfz9g4lzqmgcfk4kmxcvlxi"
, kpkgs ? import ./dep/kpkgs {}
}:

let
pactSrc = builtins.fetchTarball {
  url = "https://github.com/kadena-io/pact/archive/${pactRef}.tar.gz";
  sha256 = pactSha;
};

in kpkgs.rp.project ({ pkgs, hackGet, ... }: with pkgs.haskell.lib; {
    name = "kadena-signing-api";
    overrides = self: super: {
      pact = dontCheck ( addBuildDepend (self.callCabal2nix "pact" pactSrc {}) pkgs.z3);
    };

    packages = {
      kadena-signing-api = kpkgs.gitignoreSource ./.;
    };

    shellToolOverrides = ghc: super: {
      cabal-install = pkgs.haskellPackages.cabal-install;
      ghcid = pkgs.haskellPackages.ghcid;
    };

    shells = {
      ghc = ["kadena-signing-api"];
    };
  })
