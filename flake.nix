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
        emacs = pkgs.emacs-git-pgtk.override { withNativeCompilation = true; };
        emacs-packages =
          epkgs: with epkgs; [
            add-node-modules-path
            adoc-mode
            age
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
            consult-project-extra
            corfu
            crux
            diminish
            direnv
            disproject
            dockerfile-mode
            doom-themes
            easy-kill
            eat
            ef-themes
            elixir-mode
            embark
            embark-consult
            envrc
            eros
            exec-path-from-shell
            expreg
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
            gleam-ts-mode
            go-mode
            gruvbox-theme
            helpful
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
            pdf-tools
            persistent-scratch
            pet
            prism
            prodigy
            protobuf-mode
            pulsar
            puni
            python-pytest
            rg
            rust-mode
            selected
            shell-pop
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
