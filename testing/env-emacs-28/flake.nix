{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, emacs-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import emacs-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        emacs = pkgs.emacs28;
        emacs-packages = epkgs: with epkgs; [ vterm ];
        emacs-with-packages =
          (pkgs.emacsPackagesFor emacs).emacsWithPackages (emacs-packages);
      in with pkgs; {
        devShells.default = mkShell {
          buildInputs = [ ];
          packages = [
            emacs-with-packages

          ];
        };
      });
}
