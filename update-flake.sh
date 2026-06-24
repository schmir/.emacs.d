#!/bin/sh
# Update the flake inputs and summarize the resulting package changes with
# `nvd diff`.

set -eu

system=$(nix eval --impure --raw --expr builtins.currentSystem)
target=".#packages.${system}.default"

# Build the dev shell closure before and after updating, then diff the two.
before=$(nix build --no-link --no-warn-dirty --print-out-paths "$target")

nix flake update

after=$(nix build --no-link --no-warn-dirty --print-out-paths "$target")

nix shell nixpkgs#nvd --command nvd diff "$before" "$after"
