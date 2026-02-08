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
