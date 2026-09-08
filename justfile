set positional-arguments

# List available recipes
@_default:
    just --list

# Update flake inputs
update:
    nix flake update

# Build flake
build:
    nix build -L .

# Run emacs
run *ARGS: build
    nix run . -- {{ARGS}}

# Remove generated/temp files, cached/package dirs, and interactively git clean
clean:
    bin/clean

# Run integration tests
test: build
    nix run . -- --batch -l test/test-init.el -f ert-run-tests-batch-and-exit

# Byte-compile lisp/ and setup/, failing on any warning
check: build
    nix run . -- --batch -l test/byte-compile-check.el

# Update the flake inputs and show the resulting package changes.
flake-update-diff:
    #!/usr/bin/env bash
    set -euo pipefail
    system=$(nix eval --impure --raw --expr builtins.currentSystem)
    target=".#packages.${system}.default"
    #target=".#devShells.${system}.default"
    # Build the dev shell closure before and after updating, then diff the two.
    before=$(nix build --no-link --no-warn-dirty --print-out-paths "$target")
    # Three --quiet flags drop nix below warn level, hiding the "updating
    # lock file" notice; errors still print and still abort the recipe.
    nix flake update --no-warn-dirty --quiet --quiet --quiet
    after=$(nix build --no-link --no-warn-dirty --print-out-paths "$target")
    nix shell nixpkgs#nvd --command nvd diff "$before" "$after"
