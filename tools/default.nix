{ system ? builtins.currentSystem
, pkgs ? import <nixpkgs> {
    inherit system;
  }
, nodejs ? pkgs."nodejs-14_x"
}:
let
  node-tools = import ./node {
    inherit system pkgs nodejs;
  };
in {
  inherit node-tools;
  swagger-cli = node-tools."@apidevtools/swagger-cli";
}
