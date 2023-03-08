# Python Tools

This folder contains the Nix files used to provision the Python based tools supporting the maintenance of the signing-api.

The python-packages.nix file in this folder has been generated from the requirements.txt file by using `pip2nix` as follows:
``` bash
$(nix-build --no-out-link -A pip2nix.python39)/bin/pip2nix generate -r requirements.txt
```