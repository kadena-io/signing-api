{ kpkgs ? import ./dep/kpkgs {}}:
let
  pactSrc = kpkgs.pkgs.fetchFromGitHub {
    owner = "kadena-io";
    repo = "pact";
    rev = "853d255202d590e133aed16ee8a117b55e9bc623";
    sha256 = "1jzh1nca93z7dsm7rllfnni0lfasv10d4xm7x5yw9q37if2lssb3";
  };

  signingProject = kpkgs.rp.project ({ pkgs, hackGet, ... }: with pkgs.haskell.lib; {
    name = "kadena-signing-api";
    overrides = self: super: {
      pact = dontCheck ( addBuildDepend (self.callCabal2nix "pact" pactSrc {}) pkgs.z3);
      pact-time = dontCheck (self.callHackageDirect {
        pkg = "pact-time";
        ver = "0.2.0.0";
        sha256 = "1cfn74j6dr4279bil9k0n1wff074sdlz6g1haqyyy38wm5mdd7mr";
      } {});
    };

    packages = {
      kadena-signing-api = kpkgs.gitignoreSource ./kadena-signing-api;
      kadena-signing-api-docs = kpkgs.gitignoreSource ./kadena-signing-api-docs;
    };

    shellToolOverrides = ghc: super: {
      cabal-install = pkgs.haskellPackages.cabal-install;
      ghcid = pkgs.haskellPackages.ghcid;
    };

    shells = {
      ghc = ["kadena-signing-api" "kadena-signing-api-docs"];
    };
  });
in
  {
    inherit signingProject;
    kadena-signing-api = signingProject.ghc.kadena-signing-api;
    kadena-signing-api-docs = signingProject.ghc.kadena-signing-api-docs;
  }
