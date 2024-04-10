;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;; on lexical-binding: https://nullprogram.com/blog/2016/12/22/

(if (eq 'ns (window-system))
    (x-focus-frame nil))

(defun after-make-frame (f)
  (x-focus-frame f))

(add-hook 'after-make-frame-functions
          #'after-make-frame)

(let ((minver "28.1"))
  (when (version< emacs-version minver)
    (error "init.el: Emacs too old -- this config requires at least v%s" minver)))

;; make sure to set this before (package-initialize). Otherwise site-lisp will bail out with an
;; error when we try to set the value.
(setopt site-lisp-directory (expand-file-name "lisp" user-emacs-directory))

(setopt package-user-dir (expand-file-name "var/elpa-packages" user-emacs-directory)
        package-gnupghome-dir (expand-file-name "var/elpa-gnupg" user-emacs-directory)
        package-archives
        '(("org" . "https://orgmode.org/elpa/")
          ("melpa" . "https://melpa.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;;; Install setup.el
(load-file (expand-file-name "sup.el" site-lisp-directory))
(sup-package-install 'setup)
(require 'setup)

(setup-define :package
  (lambda (package)
    `(sup-package-install ',package))
  :documentation "Install PACKAGE if it hasn't been installed yet.
The first PACKAGE can be used to deduce the feature context."
  :repeatable t
  :shorthand #'cadr)

(setup (:package site-lisp)
  (site-lisp-initialise))

;; let's keep use-package as it's useful when trying out package, so we can copy and paste the
;; install instructions.
(setup (:package use-package)
  ;; Let imenu see `use-package' declarations
  (setopt use-package-enable-imenu-support t
          use-package-always-ensure t)
  (require 'use-package-ensure))

(require 'setup-theme)
(setup (:package no-littering)
  ;; :autoload-this
  (require 'no-littering)
  (no-littering-theme-backups)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (message "init.el: loading custom file %s" custom-file)
    (load custom-file)))

(require 'setup-core)
(setup (:package diminish))

(setup (:package exec-path-from-shell)
  (require 'exec-path-from-shell)
  (dolist (var '("DICPATH" "XDG_DATA_DIRS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(setup (:package boxquote cargo crux dockerfile-mode elixir-mode flymake-shellcheck htmlize leo lua-mode
                 prodigy solidity-flycheck tldr yaml-mode just-mode))

(setup (:package markdown-mode markdown-preview-mode)
  (setq markdown-command "multimarkdown")
  (add-to-list 'auto-mode-alist (cons "README\\.md\\'" #'gfm-mode)))

(setup (:package zoom)
  (zoom-mode))

(setup (:package flycheck flycheck-inline)
  (setq flycheck-check-syntax-automatically '(save new-line mode-enabled))
  (global-flycheck-inline-mode))

(setup (:package adoc-mode)
  (:file-match "\\.adoc$"))

(setup (:package apheleia)
  (message "Executing apheleia init code")
  (apheleia-global-mode +1)
  (with-eval-after-load 'apheleia
    (setf (alist-get 'blackzim apheleia-formatters)
          '("blackzim"))
    (when (executable-find "ruff")
      (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
      (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff)))
    (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(solidity-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(conf-toml-mode . prettier)))
  (:global "C-c b" #'apheleia-format-buffer))

(setup eldoc
  (:hook-into emacs-list-mode clojure-mode))

(setup (:package aggressive-indent)
  (:hook-into emacs-lisp-mode clojure-mode))

(setup (:package easy-kill)
  (global-set-key [remap kill-ring-save] #'easy-kill))

(setup so-long
  (setq so-long-max-lines nil
        so-long-threshold 500)
  (global-so-long-mode +1))

(setup uniquify
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 4))

;; apt install libvterm-dev libvterm-bin libtool-bin cmake
;; dnf install libvterm-devel libtool cmake
(setup (:package vterm)
  (setq vterm-max-scrollback 10000
        vterm-shell (executable-find "zsh"))
  (with-eval-after-load 'vterm
    (add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window)))
  (:hook #'compilation-shell-minor-mode)
  (:bind "C-t" #'shell-pop)
  ;; :after shell-pop
  )

(setup (:package shell-pop)
  (:global "C-t"  #'shell-pop)
  (setq shell-pop-shell-type '("vterm" "*vterm*" #'vterm)
        shell-pop-term-shell (executable-find "zsh")
        shell-pop-window-size 40))

(setup (:package eat)
  (with-eval-after-load 'eat
    (eat-eshell-mode))
  (setq eshell-visual-commands '()))

(setup (:package persistent-scratch)
  (persistent-scratch-setup-default))

(setup (:package which-key)
  (which-key-mode))

(setup (:and (executable-find "direnv") (:package direnv))
  (direnv-mode))

(setup (:package consult)
  (with-eval-after-load 'consult
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep consult-buffer
     consult-bookmark consult-recent-file consult-xref
     ;; consult--source-file
     ;; consult--source-project-file
     consult--source-bookmark
     :preview-key "M-.")))

(setup (:package marginalia)
  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  (marginalia-mode +1)
  (global-set-key [remap switch-to-buffer] 'consult-buffer))

(setup (:package  vertico)
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)
  (setq completion-styles '(basic substring))
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(setup (:package  default-text-scale)
  (:global
   "C--"  #'default-text-scale-decrease
   "C-="  #'default-text-scale-increase))

(setup (:package prism)
  (:hook-into emacs-lisp-mode clojure-mode))

(setup (:package highlight-symbol)
  (:hook-into prog-mode)
  (:option highlight-symbol-occurrence-message '(explicit))
  (:global
   [(control f3)] #'highlight-symbol
   [f3]           #'highlight-symbol-next
   [(shift f3)]   #'highlight-symbol-prev
   [(meta f3)]    #'highlight-symbol-query-replace))

(setup (:package  pulsar)
  (setq pulsar-pulse t
        pulsar-delay 0.045
        pulsar-iterations 10
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))


(setup (:package smartscan)
  (:hook-into prog-mode-hook))

(setup (:package denote)
  (:global
   "C-c n n"  #'denote
   "C-c n i"  #'denote-link-or-create
   "C-c n I"  #'denote-link
   "C-c n b"  #'denote-link-backlinks
   "C-c n a"  #'denote-add-front-matter
   "C-c n r"  #'denote-rename-file
   "C-c n R"  #'denote-rename-file-using-front-matter)
  (setq denote-directory (expand-file-name "~/m/notes")
        denote-known-keywords '("emacs" "cli" "dev" "linux" "git" "clojure" "python" "golang")))

(setup (:package orderless))

(setup (:package consult-notes)
  (:global "C-c n f" #'my/consult-notes)
  (:option consult-notes-denote-files-function (function denote-directory-text-only-files)
           consult-notes-file-dir-sources
           '(;; ("notes"             ?o "~/m//notes/")
             ("deft"      ?r "~/m/deft/")))
  (defun my/consult-notes ()
    (interactive)
    (let ((completion-styles '(orderless)))
      (consult-notes)))

  ;; (setq consult-notes-file-dir-sources '(("Name"  ?key  "path/to/dir"))) ;; Set notes dir(s), see below
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  (consult-notes-denote-mode))

;; --- setup typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setup (:package tide)
  (:option tide-completion-detailed 't
           tide-always-show-documentation 't)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(setup (:package js2-mode)
  (:file-match "\\.js\\'")
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

(setup (:package add-node-modules-path)
  (:with-mode (js-mode js2-mode)
    (:hook #'add-node-modules-path)))

(setup (:package flymake-eslint)
  (:with-mode (js-mode js2-mode)
    (:hook #'flymake-eslint-enable))

  (:option
   ;; If we don't defer the binary check, the hook will fail and dir-local.el variables will not
   ;; work.
   flymake-eslint-defer-binary-check t))



;; (if (version< emacs-version "29")
;;     (message "init.el: no eglot available in this emacs version")
;;   )

(setup (:package eglot)
  (defun my/eglot-rename (newname)
    "Rename the current symbol to NEWNAME. like eglot-rename but provides the old symbol as default."
    (interactive
     (list (read-from-minibuffer
            (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                           "unknown symbol"))
            (thing-at-point 'symbol t) nil nil nil
            (symbol-name (symbol-at-point)))))
    (eglot-rename newname))
  (:option eglot-autoshutdown t)
  (:bind  "C-c ." #'xref-find-references
          "C-c t" #'eglot-find-typeDefinition
          "C-c i" #'eglot-find-implementation
          "C-c r" #'my/eglot-rename))

(setup (:package treesit-auto)
  (:option treesit-auto-langs '(python go gomod bash yaml)
           treesit-auto-install 'prompt)
  (require 'treesit-auto)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setup (:package protobuf-mode)
  (:hook #'setup-protobuf)

  (defconst my-protobuf-style
    '((c-basic-offset . 8)
      (indent-tabs-mode . nil)))

  (defun setup-protobuf ()
    (c-add-style "my-style" my-protobuf-style t)))

(defun with-project-root-as-default-directory
    (orig-fun &rest args)
  "Run orig-fun with default-directory set to (projectile-project-root)"
  (let ((default-directory (or (projectile-project-root)
                               default-directory)))
    (apply orig-fun args)))

(defun schmir/solidity-setup ()
  ;; https://stackoverflow.com/questions/6952369/java-mode-argument-indenting-in-emacs
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     (cape-company-to-capf #'company-solidity)
                     #'cape-dabbrev)))
  (c-set-offset 'arglist-intro '+)
  (setq-local c-basic-offset 4
              tab-width 8))

(setup (:package solidity-mode company-solidity)
  (:hook #'schmir/solidity-setup)
  (with-eval-after-load 'solidity-mode
    (require 'company-solidity)
    (require 'cape)))

(setup sh-mode
  (:hook #'flymake-shellcheck-load #'flymake-mode))

(setup (:package terraform-mode))

(setup (:package nix-mode)
  (:file-match  "\\.nix\\'"))

;; configure tramp before saveplace, because it might use tramp
(setup tramp
  ;; (customize-set-variable 'tramp-syntax 'simplified)
  (setq tramp-default-method "ssh")
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-methods
                 '("yadm"
                   (tramp-login-program "yadm")
                   (tramp-login-args (("enter")))
                   (tramp-login-env (("SHELL") ("/bin/sh")))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-c"))))))

;; saveplace may need the yadm tramp method.
;; place cursor on same buffer position between editing sessions
(setup saveplace
  (save-place-mode))

(setup recentf
  (:option recentf-max-saved-items 200)
  (recentf-mode t)
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|yadm\\|su\\|sudo\\)?:")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(setup compile
  ;; scroll, but stop at first error
  (setq compilation-scroll-output 'first-error)  
  ;; colorize compile mode output
  (add-hook 'compilation-filter-hook #'display-ansi-colors))

(setup (:package ninja-mode))

(setup (:package writegood-mode)
  (:hook-into text-mode markdown-mode))

(require 'framemove-autoloads nil t)
(unless (featurep 'framemove-autoloads)
  (package-vc-install "https://github.com/emacsmirror/framemove"))
(setup (:require framemove)
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

(setup server
  (server-start))

(setup (:package  gcmh)) ;; early-init.el enables gcmh-mode

(require 'setup-mail)
(require 'setup-completion)
(require 'setup-git)
(require 'setup-smartparens)
(require 'setup-clojure)
(require 'setup-go)
(require 'setup-python)

(setup cwc
  (global-highlight-changes-mode t)
  (setq highlight-changes-visibility-initial-state nil)
  (with-eval-after-load 'whitespace
    (add-to-list 'whitespace-style 'trailing))
  (add-hook 'before-save-hook #'changed-whitespace-cleanup))

(setup git-grep
  (:global "<f5>" #'git-grep))

(dolist (mode '(eldoc-mode highlight-changes-mode))
  (diminish mode))

;; --- Configure display-buffer-alist

(setq display-buffer-alist
      '(("\\`\\*e?shell\\|compilation\\|vterm\\|Help\\*\\(?:<[[:digit:]]+>\\)?\\'"
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (reusable-frames . visible)
         (side . bottom)
         (window-height . 0.4))
        ("\\`\\*cider-repl\\|.*.clj"
         (display-buffer-reuse-window
          display-buffer-pop-up-window)
         (reusable-frames . t)
         (inhibit-switch-frames . nil))))

(defun lunaryorn-quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))

(global-set-key (kbd "C-c q") #'lunaryorn-quit-bottom-side-windows)
(global-set-key (kbd "C-c C-q") #'lunaryorn-quit-bottom-side-windows)


;;; init.el ends here
