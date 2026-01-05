;;; setup-misc.el --- Miscellaneous packages   -*- lexical-binding: t -*-

;; exec-path-from-shell: Inherit environment variables from shell
(setup (:package exec-path-from-shell)
  (require 'exec-path-from-shell)
  (dolist (var '("DICPATH" "XDG_DATA_DIRS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (dolist (var '("DIRENV_DIFF" "DIRENV_DIR" "DIRENV_FILE" "DIRENV_WATCHES"))
    (setenv var))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; Misc packages without additional configuration
(setup (:package boxquote crux htmlize leo package-lint prodigy s tldr))

;; persistent-scratch: Preserve scratch buffer across sessions
(setup (:package persistent-scratch)
  (persistent-scratch-setup-default)
  (with-current-buffer "*scratch*"
    (persistent-scratch-mode +1)))

;; goto-address-mode: Make URLs clickable in code
(setup goto-address-mode
  (:hook-into prog-mode))

;; howm: Personal wiki and note-taking system
(setup (:package howm)
  (require 'howm)
  (setq howm-history-file (expand-file-name ".howm-history" howm-directory))
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-directory))

  ;; Rename buffers to their title
  (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
  (add-hook 'after-save-hook 'howm-mode-set-buffer-name))

;; fix-project-try-vc: Workaround for project.el caching bug
(setup fix-project-try-vc
  (defun my/fix-project-try-vc (orig-fun dir)
    "Advice for project-try-vc.
project-try-fc recursively calls itself with project-vc-extra-root-markers set to nil and wrongly
caches the result of those calls via vc-file-setprop.
"
    (let ((res (cl-letf (((symbol-function 'vc-file-setprop) #'ignore))
                 (funcall orig-fun dir))))
      (when res
        (vc-file-setprop dir 'project-vc res))
      res))
  (with-eval-after-load 'project
    (advice-add 'project-try-vc :around #'my/fix-project-try-vc)))

;; project: Project management with compile and eshell bindings
(setup project
  (setq project-compilation-buffer-name-function #'project-prefixed-buffer-name)
  (setopt project-vc-extra-root-markers '(".project" ".projectile")
          project-vc-merge-submodules nil)
  (keymap-global-set "<f9>" #'project-compile)
  (keymap-global-set "S-<f9>" #'project-eshell))

;; disproject provides transient menus for managing and interacting with project files.
(setup (:package disproject)
  (keymap-set ctl-x-map "p" #'disproject-dispatch))

;; compile: Compilation buffer with ANSI colors
(setup compile
  (setq compilation-scroll-output 'first-error)
  ;; colorize compile mode output
  (add-hook 'compilation-filter-hook #'display-ansi-colors)
  ;; remove osc sequences (e.g. from ruff)
  (add-hook 'compilation-filter-hook #'ansi-osc-compilation-filter))

;; gcmh: Garbage collector magic hack for better performance
(setup (:package gcmh)) ;; early-init.el enables gcmh-mode

;; pdf-tools: Enhanced PDF viewing and annotation
(setup (:package pdf-tools)
  (add-hook 'doc-view-mode-hook #'pdf-tools-install))

;; rg: Ripgrep frontend for fast code searching
(setup (:package rg)
  (require 'rg)
  (rg-enable-default-bindings))

;; age: Age encryption with passage for auth-source
(setup (:package age
                 (passage :url "https://github.com/anticomputer/passage.el"))
  (setq age-program "rage"
        age-default-identity "~/.passage/identities"
        age-default-recipient '("~/.ssh/age_yubikey.pub"
                                "~/.ssh/age_recovery.pub"))

  (setenv "PINENTRY_PROGRAM" (or (executable-find "pinentry-mac")
                                 (executable-find "pinentry-gnome3")))

  (age-file-enable)
  ;; (auth-source-pass-enable)
  (auth-source-passage-enable)
  (setq auth-source-debug nil))

;; Helpful is an alternative to the built-in Emacs help that provides more contextual information.
(setup (:package helpful)
  (keymap-global-set  "C-h f" #'helpful-callable)
  (keymap-global-set "C-h v" #'helpful-variable)
  (keymap-global-set "C-h k" #'helpful-key)
  (keymap-global-set  "C-h x" #'helpful-command)
  (keymap-global-set "C-c C-d" #'helpful-at-point)
  (keymap-global-set "C-h F" #'helpful-function))

(provide 'setup-misc)

;;; setup-misc.el ends here
