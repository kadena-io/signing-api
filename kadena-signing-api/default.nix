{ kpkgs ? import ./dep/kpkgs {}
}:

let
pactSrc = /Users/doug/work/pact;
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
