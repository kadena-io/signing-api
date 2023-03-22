# Node Tools

This folder contains the Nix files used to provision the node-js based tools supporting the maintenance of the signing-api.

The Nix files in this folder have been generated from the node-packages.json file by using `node2nix` as follows:
``` bash
nix-shell -p node2nix --run "node2nix -i node-packages.json"
```