# Supporting Tools

This repository contains the Nix files used to provision the tools supporting the maintenance of the signing-api.

The Nix files in this folder have been generated from the node-packages.json file via using `node2nix` as follows:
``` bash
nix-shell -p node2nix --run "node2nix -i node-packages.json"
```