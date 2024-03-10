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


(add-to-list 'load-path
	     (expand-file-name "lisp" user-emacs-directory))

;; Elpaca calls an external emacs process to compile packages. It concatenates the
;; invocation-directory and invocation-name to determine the path to the emacs executable.  On nix
;; with the emacsWithPackages package, this is different to what would be found on PATH.  This
;; leads to problems during the build process. We help elpaca here a bit to find the right
;; executable.
(when (string-prefix-p "/nix/store" invocation-directory)
  (setq
   original-invocation-directory invocation-directory
   invocation-directory (expand-file-name "bin/" "~/.nix-profile/"))
  (when (string-match "/nix/store/.*-git-\\(\[0-9\]\\{8\\}\\)" original-invocation-directory)
    (setq elpaca-core-date (list (string-to-number (match-string 1 original-invocation-directory))))
    (message (format "init.el: using nix emacs, set elpaca-core-date to %s" elpaca-core-date))))

(require 'setup-elpaca)
(require 'setup-theme)
(elpaca-wait)
(require 'setup-core)

(when (file-exists-p custom-file)
  (message "init.el: loading custom file")
  (load custom-file))


(use-package boxquote :defer t)
(use-package cargo :defer t)
(use-package crux :defer t)
(use-package dockerfile-mode :defer t)
(use-package elixir-mode :defer t)
(use-package flymake-shellcheck :defer t)
(use-package htmlize :defer t)
(use-package leo :defer t)
(use-package lua-mode :defer t)
(use-package prodigy :defer tsc--dir)
(use-package solidity-flycheck :defer t)
(use-package tldr :defer t)
(use-package yaml-mode :defer t)
(use-package just-mode :defer t)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown"))

(use-package markdown-preview-mode :defer t)

(use-package zoom
  :init (zoom-mode)
  :diminish)

(use-package flycheck
  :defer t
  :init
  (setq flycheck-check-syntax-automatically '(save new-line mode-enabled)))

(use-package flycheck-inline
  :defer t
  :init
  (global-flycheck-inline-mode))

(use-package adoc-mode :ensure t :defer t
  :mode "\\.adoc$")

(use-package apheleia
  :ensure '(apheleia :host github :repo "raxod502/apheleia")
  :init
  (apheleia-global-mode +1)
  :config
  (setf (alist-get 'blackzim apheleia-formatters)
        '("blackzim"))
  (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(solidity-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(conf-toml-mode . prettier))

  :bind ("C-c b" . #'apheleia-format-buffer))

(use-builtin eldoc
  :hook ((emacs-lisp-mode clojure-mode) . eldoc-mode))

(use-package aggressive-indent
  :defer t
  :diminish
  :hook ((emacs-lisp-mode clojure-mode) . aggressive-indent-mode))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-builtin so-long
  :config
  (setq so-long-max-lines nil
        so-long-threshold 500)
  :init
  (global-so-long-mode +1))

(use-builtin uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 4))

;; apt install libvterm-dev libvterm-bin libtool-bin cmake
;; dnf install libvterm-devel libtool cmake
(eval
 `(use-package vterm
    :ensure ,(not (featurep 'vterm-autoloads))
    :init
    (setq vterm-max-scrollback 10000
          vterm-shell (executable-find "zsh"))
    :config
    (add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window))
    (add-hook 'vterm-mode-hook #'compilation-shell-minor-mode)
    :bind (:map vterm-mode-map
                ("C-t" . #'shell-pop))
    :after shell-pop))

(use-package shell-pop
  :defer t
  :bind
  ("C-t" . #'shell-pop)
  :init
  (setq shell-pop-shell-type '("vterm" "*vterm*" #'vterm)
        shell-pop-term-shell (executable-find "zsh")
        shell-pop-window-size 40))


(use-package persistent-scratch :demand t
  :config
  (persistent-scratch-setup-default))

(use-package which-key :demand t
  :diminish
  :config
  (which-key-mode))

(use-package projectile :demand t
  :disabled
  :diminish
  :init
  (setq-default projectile-completion-system 'default)

  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  :bind
  (("<f9>" . #'projectile-compile-project)))

(use-package direnv :demand t
  :if (executable-find "direnv")
  :config
  (direnv-mode))

(use-package consult :demand t
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-buffer
   consult-bookmark consult-recent-file consult-xref
   ;; consult--source-file
   ;; consult--source-project-file
   consult--source-bookmark
   :preview-key "M-."))

(use-package marginalia :demand t
  :init
  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))

  :config
  (marginalia-mode +1)
  (global-set-key [remap switch-to-buffer] 'consult-buffer))


(use-package vertico
  :init
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


(use-package default-text-scale
  :bind
  ("C--" . #'default-text-scale-decrease)
  ("C-=" . #'default-text-scale-increase))

(use-package prism
  :hook ((emacs-lisp-mode clojure-mode) . prism-mode))

(use-package highlight-symbol
  :diminish
  :hook
  (prog-mode . highlight-symbol-mode)
  :config
  (defadvice highlight-symbol-count (around turn-off-symbol-counting activate)
    (interactive))
  :bind
  ([(control f3)] . #'highlight-symbol)
  ([f3] . #'highlight-symbol-next)
  ([(shift f3)] . #'highlight-symbol-prev)
  ([(meta f3)] . #'highlight-symbol-query-replace))

(use-package pulsar
  :init
  (setq pulsar-pulse t
        pulsar-delay 0.045
        pulsar-iterations 10
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)
  :config
  (pulsar-global-mode 1))


(use-package smartscan
  :init
  (add-hook 'prog-mode-hook #'smartscan-mode))

(use-package denote
  :bind
  (("C-c n n" . denote)
   ("C-c n i" . denote-link-or-create)
   ("C-c n I" . denote-link)
   ("C-c n b" . denote-link-backlinks)
   ("C-c n a" . denote-add-front-matter)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter))
  :init
  (setq denote-directory (expand-file-name "~/m/notes")
        denote-known-keywords '("emacs" "cli" "dev" "linux" "git" "clojure" "python" "golang")))

(use-package orderless
  :demand t)

(use-package consult-notes
  :bind (("C-c n f" . #'my/consult-notes))
  :config
  (defun my/consult-notes ()
    (interactive)
    (let ((completion-styles '(orderless)))
      (consult-notes)))
  (setq consult-notes-file-dir-sources
        '(;; ("notes"             ?o "~/m//notes/")
          ("deft"      ?r "~/m/deft/")))

  ;; (setq consult-notes-file-dir-sources '(("Name"  ?key  "path/to/dir"))) ;; Set notes dir(s), see below
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  (consult-notes-denote-mode)

  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function (function denote-directory-text-only-files)))

;; (use-package denote-menu)
;; --- setup typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :init
  (setq tide-completion-detailed 't
        tide-always-show-documentation 't)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node")

(use-package add-node-modules-path
  :hook (js-mode . add-node-modules-path))

(use-package flymake-eslint
  :hook (js-mode . flymake-eslint-enable)
  :init
  ;; If we don't defer the binary check, the hook will fail and dir-local.el variables will not
  ;; work.
  (setq flymake-eslint-defer-binary-check t))

(if (version< emacs-version "29")
    (message "init.el: no eglot available in this emacs version")
  (use-builtin eglot
    :defer t
    :init
    (defun my/eglot-rename (newname)
      "Rename the current symbol to NEWNAME. like eglot-rename but provides the old symbol as default."
      (interactive
       (list (read-from-minibuffer
              (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                             "unknown symbol"))
              (thing-at-point 'symbol t) nil nil nil
              (symbol-name (symbol-at-point)))))
      (eglot-rename newname))
    :custom
    (eglot-autoshutdown t)
    :bind (:map eglot-mode-map
                ("C-c ." . #'xref-find-references)
                ("C-c t" . #'eglot-find-typeDefinition)
                ("C-c i" . #'eglot-find-implementation)
                ("C-c r" . #'my/eglot-rename))))

(use-package treesit-auto
  :custom
  (treesit-auto-langs '(python go gomod bash yaml))
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package protobuf-mode
  :defer t
  :config
  (progn
    (defconst my-protobuf-style
      '((c-basic-offset . 8)
        (indent-tabs-mode . nil)))

    (defun setup-protobuf ()
      (c-add-style "my-style" my-protobuf-style t))

    (add-hook 'protobuf-mode-hook #'setup-protobuf)))

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

(use-package company-solidity
  :defer t
  :after solidity-mode)

(use-package solidity-mode
  :defer t
  :config
  (require 'company-solidity)
  (require 'cape)
  (add-hook 'solidity-mode-hook #'schmir/solidity-setup))

(use-builtin sh-script
  :defer t
  :config
  (progn
    (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
    (add-hook 'sh-mode-hook 'flymake-mode)))

(use-package terraform-mode :defer t)
(use-package nix-mode
  :defer t
  :mode "\\.nix\\'")

;; configure tramp before saveplace, because it might use tramp
(use-builtin tramp
  :config
  ;; (customize-set-variable 'tramp-syntax 'simplified)
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))

;; saveplace may need the yadm tramp method.
;; place cursor on same buffer position between editing sessions
(use-builtin saveplace :demand t :after tramp
  :config
  (save-place-mode))

(use-builtin recentf
  :custom
  (recentf-max-saved-items 200)
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|yadm\\|su\\|sudo\\)?:")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)  
  (recentf-mode t))

(use-builtin compile
  :defer t
  :init
  ;; scroll, but stop at first error
  (setq compilation-scroll-output 'first-error)
  :config
  ;; colorize compile mode output
  (add-hook 'compilation-filter-hook #'display-ansi-colors))

(use-package ninja-mode :defer t)

(use-package writegood-mode
  :defer t
  :init
  (progn
    (add-hook 'text-mode-hook #'writegood-mode)
    (add-hook 'markdown-mode-hook #'writegood-mode))
  :bind (("C-c g" . #'writegood-mode)))

(use-package framemove :demand t
  :ensure (:host "github.com" :repo "emacsmirror/framemove")
  :config
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

(use-builtin server :demand t
  :config
  (server-start))

(use-package gcmh
  :diminish) ;; early-init.el enables gcmh-mode

(require 'setup-mail)
(require 'setup-completion)
(require 'setup-git)
(require 'setup-cwc)
(require 'setup-smartparens)
(require 'setup-clojure)
(require 'setup-go)
(require 'setup-python)

(autoload 'git-grep "git-grep")
(global-set-key (kbd "<f5>") #'git-grep)

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
