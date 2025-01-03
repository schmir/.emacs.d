# List available recipes
default:
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
