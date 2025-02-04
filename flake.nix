{
  inputs = {
    sync.url = "github:schmir/sync-flake";
    nixpkgs.follows = "sync/nixpkgs";
    flake-utils.follows = "sync/flake-utils";
    emacs-overlay.follows = "sync/emacs-overlay";
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
        emacs = pkgs.emacs-pgtk;
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
            flymake-clippy
            flymake-eslint
            flymake-kondor
            flymake-shellcheck
            gcmh
            git-gutter
            git-link
            git-messenger
            go-mode
            gruvbox-theme
            highlight-symbol
            howm
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
            rust-mode
            pdf-tools
            persistent-scratch
            pet
            prism
            prodigy
            protobuf-mode
            pulsar
            puni
            python-pytest
            #setup
            shell-pop
            #site-lisp
            smartscan
            solidity-flycheck
            spacemacs-theme
            super-save
            tempel
            terraform-mode
            tldr
            treesit-auto
            treesit-grammars.with-all-grammars
            vertico
            vterm
            which-key
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
            (callPackage ./eglot-booster.nix {
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
