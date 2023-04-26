;;; setup-core --- configure core setttings     -*- lexical-binding: t -*-

;;; Code:

;; silence warnings, especially from native compilation
(setq warning-minimum-level :error)

(advice-add 'risky-local-variable-p :override #'ignore)

;; prevent emacs from asking for coding-system...
(set-language-environment "utf-8")
(global-set-key (kbd "<f12>") 'toggle-menu-bar-mode-from-frame)
(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

(if (fboundp #'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode))

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; when on a tab, make the cursor the tab length
(setq-default x-stretch-cursor t)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(global-set-key (kbd "C-z") #'undo)

(setq-default fill-column 80)
(add-hook 'prog-mode-hook (lambda()
                            (setq fill-column 99)))
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)

(global-auto-revert-mode 1)
(auto-image-file-mode 1)
(column-number-mode 1)

(recentf-mode t)
(setq recentf-max-saved-items 200)

(setq mark-even-if-inactive t)
(transient-mark-mode 1)

(auto-compression-mode t)

(setq line-move-visual nil)

;; Increase the amount of data which Emacs reads from the process (recommended by lsp mode's doc)
(setq read-process-output-max (* 1024 1024))
(setq change-major-mode-with-file-name t
      create-lockfiles nil
      x-select-enable-primary t  ;; after mouse selection in X11, you can paste by `yank' in emacs
      save-interprogram-paste-before-kill t
      ;; Filename completion ignores these.
      completion-ignored-extensions (append completion-ignored-extensions
                                            '(".pyc" ".o" ".so" ".os" ".cmi" ".cmx" ".rsm" ".rsr"))
      backward-delete-char-untabify-method 'nil	;; don´t untabify, just delete one char
      font-lock-maximum-decoration t			;; maximum decoration
      next-line-add-newlines nil			;; don´t add newlines when trying to move cursor behind eof
      show-paren-style 'expression
      default-indicate-empty-lines t
      line-number-display-limit-width 100000
      kill-whole-line t				;; make kill-line at beginning of line kill the whole line
      woman-use-own-frame nil				;; don't create new frame for manpages
      vc-handled-backends '(Git Hg)
      vc-follow-symlinks t				;; follow symlinks and don't ask
      enable-recursive-minibuffers t)


(defalias 'br 'boxquote-region)
(defalias 'cc 'cider-connect)
(defalias 'sbke 'save-buffers-kill-emacs)
(defalias 'g 'gnus)
(defalias 'ee 'eval-expression)
(defalias 'rb 'revert-buffer)
(setq send-mail-function 'message-send-mail-with-sendmail
      message-send-mail-function 'message-send-mail-with-sendmail
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header
      gnus-init-file (expand-file-name "~/.gnus-init.el"))

;; we substitute sendmail with msmtp if it's installed
(let ((msmtp (executable-find "msmtp")))
  (when msmtp
    (setq sendmail-program msmtp)))

(defun untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

;; let me use windmove keybindings even in org-mode
(setq org-replace-disputed-keys t)

(global-set-key (kbd "S-SPC") (lambda() (interactive) (cycle-spacing -1)))

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)

;; mouse avoidance mode is buggy, see
;; https://groups.google.com/g/gnu.emacs.help/c/W_1VhwJrelE
;; (mouse-avoidance-mode 'banish)

;; (setq make-pointer-invisible nil)

;; Configure hippie-expand
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(global-set-key (kbd "<C-tab>") #'hippie-expand)


(provide 'setup-core)
;;; setup-core.el ends here
