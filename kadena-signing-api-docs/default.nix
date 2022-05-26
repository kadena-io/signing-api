{ compiler ? "ghc881"
, rev      ? "c4f97342ba8ac84def72328616dd05d005bb4715"
, sha256   ? "1p2gbisib2jrz4r9b5vzfvmirgmz9sr2ksalngaw908vvg9hsvai"
, pkgs     ?
    import (builtins.fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256; }) {
      config.allowBroken = false;
      config.allowUnfree = true;
    }
}:
let gitignoreSrc = import (pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "2ced4519f865341adcb143c5d668f955a2cb997f";
      sha256 = "0fc5bgv9syfcblp23y05kkfnpgh3gssz6vn24frs8dzw39algk2z";
    }) {};

in
pkgs.haskell.packages.${compiler}.developPackage {
  name = builtins.baseNameOf ./.;
  root = gitignoreSrc.gitignoreSource ./.;

  overrides = self: super: with pkgs.haskell.lib; {
    # Don't run a package's test suite
    # foo = dontCheck super.foo;
    #
    # Don't enforce package's version constraints
    # bar = doJailbreak super.bar;

    megaparsec = dontCheck (self.callHackageDirect {
      pkg = "megaparsec";
      ver = "9.0.0";
      sha256 = "03kqcfpsmi5l4mr6lsmlpks2mp9prf9yy97mmrkclwqpxybdjx2l";
    } {});
    neat-interpolation = dontCheck (self.callHackageDirect {
      pkg = "neat-interpolation";
      ver = "0.5.1.2";
      sha256 = "0lcgjxw690hyswqxaghf7z08mx5694l7kijyrsjd42yxswajlplx";
    } {});
    pact = dontCheck super.pact;
    pact-time = dontCheck (self.callHackageDirect {
      pkg = "pact-time";
      ver = "0.2.0.0";
      sha256 = "1cfn74j6dr4279bil9k0n1wff074sdlz6g1haqyyy38wm5mdd7mr";
    } {});
    prettyprinter = dontCheck (self.callHackageDirect {
      pkg = "prettyprinter";
      ver = "1.6.0";
      sha256 = "0f8wqaj3cv3yra938afqf62wrvq20yv9jd048miw5zrfavw824aa";
    } {});
    sbv = dontCheck (self.callHackageDirect {
      pkg = "sbv";
      ver = "8.8";
      sha256 = "0sbl7xczk7qn658j4zcp7wg0x9gxy07wqxv7xnjhzrjx066qjix1";
    } {});
    unordered-containers = dontCheck (self.callHackageDirect {
      pkg = "unordered-containers";
      ver = "0.2.15.0";
      sha256 = "101fjg7jsa0mw57clpjwc2vgrdkrnn0vmf4xgagja21ynwwbl2b5";
    } {});

    # To discover more functions that can be used to modify haskell
    # packages, run "nix-repl", type "pkgs.haskell.lib.", then hit
    # <TAB> to get a tab-completed list of functions.
  };
  source-overrides = {
    # Use a specific hackage version using callHackage. Only works if the
    # version you want is in the version of all-cabal-hashes that you have.
    # bytestring = "0.10.8.1";

    kadena-signing-api = ../kadena-signing-api;

    # Use a particular commit from github
    pact = pkgs.fetchFromGitHub
      { owner = "kadena-io";
        repo = "pact";
        rev = "1bf68ea8f5c009c136ff0df008a4fc30d775886f";
        sha256 = "1338qs533crzfqnd4w38xkcimlxidalhva2x89v84m382n98vj8k";
      };
  };
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.haskell.packages.${compiler}.cabal-install
      pkgs.haskell.packages.${compiler}.ghcid
    ];
  });
}
