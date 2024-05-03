{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.emacs-overlay = {
    url = "github:nix-community/emacs-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      emacs-overlay,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import emacs-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        emacs = pkgs.emacs-git;
        emacs-packages =
          epkgs: with epkgs; [
            add-node-modules-path
            adoc-mode
            aggressive-indent
            anti-zenburn-theme
            apheleia
            boxquote
            cape
            cargo
            cider
            clojure-mode
            clojure-mode-extra-font-locking
            clojure-ts-mode
            company
            company-solidity
            consult
            consult-dir
            consult-notes
            consult-project-extra
            corfu
            crux
            default-text-scale
            denote
            diminish
            direnv
            dockerfile-mode
            easy-kill
            eat
            ebdb
            ef-themes
            elixir-mode
            envrc
            eros
            exec-path-from-shell
            flycheck
            flycheck-clj-kondo
            flycheck-inline
            flycheck-package
            flymake-eslint
            flymake-kondor
            flymake-ruff
            flymake-shellcheck
            gcmh
            git-gutter
            git-link
            git-messenger
            go-mode
            gruvbox-theme
            highlight-symbol
            htmlize
            just-mode
            kaolin-themes
            leo
            leuven-theme
            lua-mode
            macrostep
            magit
            marginalia
            markdown-mode
            markdown-preview-mode
            ninja-mode
            nix-mode
            nix-ts-mode
            no-littering
            orderless
            package-lint
            persistent-scratch
            prism
            prodigy
            protobuf-mode
            pulsar
            puni
            python-pytest
            setup
            shell-pop
            site-lisp
            smartparens
            smartscan
            solidity-flycheck
            spacemacs-theme
            super-save
            tempel
            tempel-collection
            terraform-mode
            tldr
            treesit-auto
            treesit-grammars.with-all-grammars
            vertico
            vterm
            which-key
            writegood-mode
            yaml-mode
            zenburn-theme
            zerodark-theme
            zoom
            zoxide
            (callPackage ./framemove.nix {
              emacs = emacs;
              inherit (pkgs) fetchFromGitHub lib;
              inherit (epkgs) trivialBuild;
            })
          ];
        emacs-with-packages = (pkgs.emacsPackagesFor emacs).emacsWithPackages (emacs-packages);
        emacs-nox = (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages (emacs-packages);
      in
      {
        apps.default = {
          type = "app";
          program = "${emacs-with-packages}/bin/emacs";
        };
        defaultPackage = emacs-with-packages;
        packages.default = emacs-with-packages;
        packages.emacs-nox = emacs-nox;
        packages.naked = pkgs.emacs;
        packages.emacs28 = pkgs.emacs28;
      }
    );
}
