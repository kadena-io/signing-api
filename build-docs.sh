#!/bin/sh

# NOTE: You must have installed spectacle, a swagger doc generator in order
# for this script to work.  See also:
#
# https://github.com/sourcey/spectacle
# https://sourcey.com/spectacle

nix-build
result/bin/gen-docs > swagger.json
spectacle -t docs swagger.json
