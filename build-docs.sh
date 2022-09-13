#!/usr/bin/env sh

# NOTE: You must have installed spectacle, a swagger doc generator in order
# for this script to work.  See also:
#
# https://github.com/sourcey/spectacle
# https://sourcey.com/spectacle

nix-build -A kadena-signing-api-docs
result/bin/gen-docs > swagger.json

# Get via `npm install spectacle-docs` 
# spectacle -t docs swagger.json
