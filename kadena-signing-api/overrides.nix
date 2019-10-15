pactSrc: pkgs: self: super: with pkgs.haskell.lib;
let # Working on getting this function upstreamed into nixpkgs, but
    # this actually gets things directly from hackage and doesn't
    # depend on the state of nixpkgs.  Should allow us to have fewer
    # github overrides.
    callHackageDirect = {pkg, ver, sha256}@args:
      let pkgver = "${pkg}-${ver}";
      in self.callCabal2nix pkg (pkgs.fetchzip {
           url = "http://hackage.haskell.org/package/${pkgver}/${pkgver}.tar.gz";
           inherit sha256;
         }) {};

    ourOverrides = {
      pact = dontCheck ( addBuildDepend (self.callCabal2nix "pact" pactSrc {}) pkgs.z3);

    };
in

(import "${pactSrc}/overrides.nix" pkgs self super) // ourOverrides
