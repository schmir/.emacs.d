;;; setup-languages.el --- Programming language configurations   -*- lexical-binding: t -*-

(require 'treesit)

;; Packages used by language profiles or their related editing commands.
(setup (:package add-node-modules-path
                 cargo
                 consult-eglot
                 eglot
                 (eglot-booster :url "https://github.com/jdtsmith/eglot-booster.git")
                 elixir-mode
                 (fm-ruff :url "https://github.com/schmir/fm-ruff")
                 flymake-clippy
                 flymake-eslint
                 flymake-shellcheck
                 janet-mode
                 just-mode
                 lua-mode
                 ninja-mode
                 nix-mode
                 nix-ts-mode
                 pet
                 python-pytest
                 rust-mode
                 terraform-mode))

;; markdown-mode: Editing and previewing markdown files
(setup (:package markdown-mode markdown-preview-mode)
  (setq markdown-command "multimarkdown"))

;; adoc-mode: Editing AsciiDoc files
(setup (:package adoc-mode)
  (:match-file "\\.adoc$"))

(setup (:package protobuf-mode))

(provide 'setup-languages)

;;; setup-languages.el ends here
