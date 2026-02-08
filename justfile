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

# Run integration tests
test: build
    nix run . -- --batch -l test/test-init.el -f ert-run-tests-batch-and-exit
