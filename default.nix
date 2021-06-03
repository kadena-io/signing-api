{ kpkgs ? import ./dep/kpkgs {}}:
let
  pactSrc = kpkgs.pkgs.fetchFromGitHub {
    owner = "kadena-io";
    repo = "pact";
    rev = "4971ab6078b75eb612d83d56f1e7cd139a5a2ba8";
    sha256 = "1jx1hd596r5rrx96r2v2xds6pjjmi4lfk7xm14f3gkx2gmavgyr3";
  };

  signingProject = kpkgs.rp.project ({ pkgs, hackGet, ... }: with pkgs.haskell.lib; {
    name = "kadena-signing-api";
    overrides = self: super: {
      pact = dontCheck ( addBuildDepend (self.callCabal2nix "pact" pactSrc {}) pkgs.z3);
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
