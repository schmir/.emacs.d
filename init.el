;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;; on lexical-binding: https://nullprogram.com/blog/2016/12/22/

;; focus initial frame, need this for macos
(if (eq 'ns (window-system))
    (x-focus-frame nil))
(add-hook 'after-make-frame-functions #'x-focus-frame)

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "init.el: Emacs too old -- this config requires at least v%s" minver)))

;;; Initialize lisp directory
(progn
  (setopt site-lisp-directory (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path site-lisp-directory)
  (require 'setup-lisp-directory)
  (setup-lisp-directory))

(require 'setup-setup)
(require 'setup-ui)
(require 'setup-core)
(require 'setup-editing)
(require 'setup-languages)

(let* ((home "/home")
       (truename (file-truename home)))
  (when (not (string= home truename))
    (add-to-list 'directory-abbrev-alist (cons (concat "\\`" truename)  home))))

(defalias 'yes-or-no-p 'y-or-n-p)  ;; y/n is enough

;; some aliases for interactive use with M-x
(defalias 'br 'boxquote-region)
(defalias 'cc 'cider-connect)
(defalias 'sbke 'save-buffers-kill-emacs)
(defalias 'g 'gnus)
(defalias 'ee 'eval-expression)
(defalias 'rb 'revert-buffer)

;; exec-path-from-shell: Inherit environment variables from shell
(setup (:package exec-path-from-shell)
  (require 'exec-path-from-shell)
  (dolist (var '("DICPATH" "XDG_DATA_DIRS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (dolist (var '("DIRENV_DIFF" "DIRENV_DIR" "DIRENV_FILE" "DIRENV_WATCHES"))
    (setenv var))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; Various packages without additional configuration
(setup (:package boxquote cargo crux dockerfile-mode elixir-mode flymake-shellcheck htmlize just-mode leo lua-mode
                 ninja-mode package-lint prodigy s solidity-flycheck terraform-mode tldr yaml-mode))

;; zoom: Auto-resize windows to golden ratio
(setup (:package zoom)
  (zoom-mode))

;; flycheck: On-the-fly syntax checking with inline error display
(setup (:package flycheck flycheck-inline flycheck-package)
  (setq flycheck-check-syntax-automatically '(save new-line mode-enabled))
  (global-flycheck-inline-mode))

;; persistent-scratch: Preserve scratch buffer across sessions
(setup (:package persistent-scratch)
  (persistent-scratch-setup-default)
  (with-current-buffer "*scratch*"
    (persistent-scratch-mode +1)))

;; which-key: Show available keybindings in popup
(setup (:package which-key)
  (which-key-mode))

;; envrc: Direnv integration for per-directory environments
(setup (:and (executable-find "direnv") (:package envrc))
  (require 'envrc)
  (keymap-set envrc-mode-map "C-c e" #'envrc-command-map)
  (add-hook 'after-init-hook #'envrc-global-mode))

;; text-scaling: Keybindings for adjusting font size
(setup text-scaling
  (keymap-global-set "C--"  #'text-scale-decrease)
  (keymap-global-set "C-="  #'text-scale-increase)
  (setq global-text-scale-adjust-resizes-frames nil))

;; goto-address-mode: Make URLs clickable in code
(setup goto-address-mode
  (:hook-into prog-mode))

;; pulsar: Pulse highlight line after navigation
(setup (:package pulsar)
  (setq pulsar-pulse t
        pulsar-delay 0.045
        pulsar-iterations 10
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))


;; howm: Personal wiki and note-taking system
(setup (:package howm)
  (require 'howm)
  (setq howm-history-file (expand-file-name ".howm-history" howm-directory))
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-directory))

  ;; Rename buffers to their title
  (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
  (add-hook 'after-save-hook 'howm-mode-set-buffer-name))


;; super-save: Auto-save buffers on focus loss
(setup (:package super-save)
  (super-save-mode +1))

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
  (setopt project-vc-extra-root-markers '(".project" ".projectile")
          project-vc-merge-submodules nil)
  (keymap-global-set "<f9>" #'project-compile)
  (keymap-global-set "S-<f9>" #'project-eshell))

;; zoxide: Smart directory tracking and jumping
(setup (:and (executable-find "zoxide")
             (:package zoxide))
  (defun my/zoxide-add
      ()
    (zoxide-add)
    (when-let* ((proj (project-current))
                (root (project-root proj))
                (path (expand-file-name root)))
      (message "add project-root %s to zoxide" path)
      (zoxide-add path)))

  (add-hook 'find-file-hook #'my/zoxide-add))

;; compile: Compilation buffer with ANSI colors
(setup compile
  (setq compilation-scroll-output 'first-error)
  ;; colorize compile mode output
  (add-hook 'compilation-filter-hook #'display-ansi-colors))


;; gcmh: Garbage collector magic hack for better performance
(setup (:package gcmh)) ;; early-init.el enables gcmh-mode

;; pdf-tools: Enhanced PDF viewing and annotation
(setup (:package pdf-tools)
  (add-hook 'doc-view-mode-hook #'pdf-tools-install))

(require 'setup-mail)
(require 'setup-completion)
(require 'setup-git)
(require 'setup-lisp)
(require 'setup-go)
(require 'setup-python)
(require 'setup-shell)

;; cwc: Run whitespace-cleanup only for changed lines
(setup cwc
  (my/run-when-display-initialized
   (lambda()
     (message "init.el: initializing cwc-global-mode")
     (cwc-global-mode +1))))


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


;;; init.el ends here
