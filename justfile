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
run: build
    nix run .
