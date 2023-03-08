{ kpkgs ? import ./dep/kpkgs {}
, rev ? "22.11"
, sha256 ? "11w3wn2yjhaa5pv20gbfbirvjq6i3m7pqrq2msf0g7cv44vijwgw"
, pkgs ? import (builtins.fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256;
  }) {
    config.allowBroken = false;
    config.allowUnfree = true;
  }
}:
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
      direct-sqlite = dontCheck (self.callHackageDirect {
        pkg = "direct-sqlite";
        ver = "2.3.26";
        sha256 = "1kdkisj534nv5r0m3gmxy2iqgsn6y1dd881x77a87ynkx1glxfva";
      } {});
      generic-arbitrary = dontCheck (self.callHackageDirect {
        pkg = "generic-arbitrary";
        ver = "1.0.1";
        sha256 = "sha256-gET1SkgjtU+7uvCaZ7NNpDdQX7n5rzze243Y/LH2F7M=";
      } {});
    };

    packages = {
      kadena-signing-api = kpkgs.gitignoreSource ./kadena-signing-api;
      kadena-signing-api-mock = kpkgs.gitignoreSource ./kadena-signing-api-mock;
    };

    shellToolOverrides = ghc: super: {
      cabal-install = pkgs.haskellPackages.cabal-install;
      ghcid = pkgs.haskellPackages.ghcid;
      spectacle = pkgs.spectacle;
    };

    shells = {
      ghc = ["kadena-signing-api" "kadena-signing-api-mock"];
    };
  });
  tools = import ./tools { inherit pkgs; };
  inherit (signingProject.ghc) kadena-signing-api kadena-signing-api-mock;
  yq = pkgs.yq-go;
  schema-tests = import ./schema-tests.nix {
    inherit pkgs;
    inherit (tools) schemathesis;
    inherit kadena-signing-api-mock;
  };
in
  {
    inherit
      signingProject
      kadena-signing-api
      kadena-signing-api-mock
      tools
      yq
      schema-tests
    ;
    inherit (tools) swagger-cli schemathesis;
  }
