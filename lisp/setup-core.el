;;; setup-core --- configure core setttings     -*- lexical-binding: t -*-

;;; Code:

(let* ((home "/home")
       (truename (file-truename home)))
  (when (not (string= home truename))
    (add-to-list 'directory-abbrev-alist (cons (concat "\\`" truename)  home))))

(defalias 'yes-or-no-p 'y-or-n-p)  ;; y/n is enough

;; Show lisp names in customzize interface
(setq custom-unlispify-tag-names nil)


;; silence warnings, especially from native compilation
(setq warning-minimum-level :error)

(advice-add 'risky-local-variable-p :override #'ignore)

;; prevent emacs from asking for coding-system...
(set-language-environment "utf-8")
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)

(setq explicit-shell-file-name (executable-find "zsh"))

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

(when-let* ((exe (executable-find "hunspell")))
  (setq ispell-program-name exe)
  (message (format "setup-core.el: using %s for ispell" exe)))

;; Make sure we have a sensible word list. Otherwise corfu may complain it's missing, which
;; happened in git-commit mode to me. The root cause is that a call to ispell-lookup-words, e.g.
;;
;;   (ispell-lookup-words "an")
;;
;; failed.
(setq ispell-complete-word-dict
      (expand-file-name "google-10000-english-no-swears.txt" user-emacs-directory))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)




(add-hook 'before-save-hook #'time-stamp)



(global-auto-revert-mode 1)
(auto-image-file-mode 1)
(column-number-mode 1)

(setq read-file-name-completion-ignore-case t)

(setq mark-even-if-inactive t)
(transient-mark-mode 1)

(auto-compression-mode t)

(setq line-move-visual nil)

;; Increase the amount of data which Emacs reads from the process (recommended by lsp mode's doc)
(setq read-process-output-max (* 1024 1024))
(setq change-major-mode-with-file-name t
      create-lockfiles nil
      x-select-enable-primary t                 ;; after mouse selection in X11, you can paste by `yank' in emacs
      save-interprogram-paste-before-kill t
      ;; Filename completion ignores these.
      completion-ignored-extensions (append completion-ignored-extensions
                                            '(".pyc" ".o" ".so" ".os" ".cmi" ".cmx" ".rsm" ".rsr"))
      backward-delete-char-untabify-method 'nil	;; don´t untabify, just delete one char
      font-lock-maximum-decoration t		;; maximum decoration
      next-line-add-newlines nil		;; don´t add newlines when trying to move cursor behind eof
      default-indicate-empty-lines t
      line-number-display-limit-width 100000
      kill-whole-line t				;; make kill-line at beginning of line kill the whole line
      woman-use-own-frame nil			;; don't create new frame for manpages
      vc-follow-symlinks t			;; follow symlinks and don't ask
      enable-recursive-minibuffers t)

(setq epg-pinentry-mode 'loopback)


(keymap-global-set "<backtab>" (lambda() (interactive) (cycle-spacing -1)))

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; let me type umlauts with right option + u and do not mess with the left option key
(if (boundp 'ns-alternate-modifier)
    (setq ns-alternate-modifier 'meta
          ns-right-alternate-modifier 'none))

;; some-global-keybindings: Common key overrides
(setup some-global-keybindings
  (keymap-global-set "C-z" #'undo)
  (keymap-global-set "<f12>" #'toggle-menu-bar-mode-from-frame)
  (keymap-global-set "C-g" #'prot/keyboard-quit-dwim))

;; so-long: Handle files with very long lines gracefully
(setup so-long
  (setq so-long-max-lines nil
        so-long-threshold 500)
  (global-so-long-mode +1))

;; uniquify: Disambiguate buffer names with path prefixes
(setup uniquify
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 4))

;; tramp: Remote file editing with yadm support
(setup tramp
  ;; (customize-set-variable 'tramp-syntax 'simplified)
  (setq tramp-default-method "ssh"
        password-cache-expiry (* 90 60))
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-methods
                 '("yadm"
                   (tramp-login-program "yadm")
                   (tramp-login-args (("enter")))
                   (tramp-login-env (("SHELL") ("/bin/sh")))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-c"))))))

;; saveplace: Restore cursor position when reopening files
;; saveplace may need the yadm tramp method.
;; place cursor on same buffer position between editing sessions
(setup saveplace
  (save-place-mode))

;; recentf: Track recently opened files
(setup recentf
  (:option recentf-max-saved-items 200)
  (recentf-mode t)
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|yadm\\|su\\|sudo\\)?:")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; savehist: Persist minibuffer history across sessions
(setup savehist
  (setq history-length 10000
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

;; server: Enable emacsclient connections
(setup server
  (server-start))

(provide 'setup-core)
;;; setup-core.el ends here
