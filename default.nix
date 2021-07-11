{ kpkgs ? import ./dep/kpkgs {}}:
let
  nix-thunk-src = (kpkgs.pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "nix-thunk";
    rev = "bab7329163fce579eaa9cfba67a4851ab806b76f";
    sha256 = "0wn96xn6prjzcsh4n8p1n40wi8la53ym5h2frlqbfzas7isxwygg";
  });
  inherit (import nix-thunk-src {}) thunkSource;

  pactSrc = thunkSource ./dep/pact ;

  signingProject = kpkgs.rp.project ({ pkgs, hackGet, ... }: with pkgs.haskell.lib; {
    name = "kadena-signing-api";
    overrides = self: super: {
      pact = dontCheck (addBuildDepend (self.callCabal2nix "pact" pactSrc {}) pkgs.z3);
      pact-time = dontCheck (self.callHackageDirect {
        pkg = "pact-time";
        ver = "0.2.0.0";
        sha256 = "1cfn74j6dr4279bil9k0n1wff074sdlz6g1haqyyy38wm5mdd7mr";
      } {});
      trasa = dontCheck (self.callHackageDirect {
        pkg = "trasa";
        ver = "0.4.1";
        sha256 = "1j8kpp4qhnpgnyn7ix1ywxwgnhs02ap20mrhx3c1fbapr2ichl8s";
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
