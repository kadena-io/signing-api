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
  python-tools = import ./python {
    inherit system pkgs;
  };
in {
  inherit node-tools python-tools;
  swagger-cli = node-tools."@apidevtools/swagger-cli";
  schemathesis = python-tools.schemathesis;
}
