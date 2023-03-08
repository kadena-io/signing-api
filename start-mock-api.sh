#! /bin/bash

$(nix-build -A kadena-signing-api-mock --no-out-link)/bin/kadena-signing-api-mock
