;;; setup-core --- configure core setttings     -*- lexical-binding: t -*-

;;; Code:

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

(provide 'setup-core)
;;; setup-core.el ends here
