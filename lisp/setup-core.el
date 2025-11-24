;;; setup-core --- configure core setttings     -*- lexical-binding: t -*-

;;; Code:

;; Show lisp names in customzize interface
(setq custom-unlispify-tag-names nil)

(setq mouse-yank-at-point t)

;; silence warnings, especially from native compilation
(setq warning-minimum-level :error)

(advice-add 'risky-local-variable-p :override #'ignore)

;; prevent emacs from asking for coding-system...
(set-language-environment "utf-8")
(global-set-key (kbd "<f12>") 'toggle-menu-bar-mode-from-frame)
(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

(setq explicit-shell-file-name (executable-find "zsh"))

(if (fboundp #'pixel-scroll-precision-mode)
    (progn
      (setq pixel-scroll-precision-interpolate-page t
            ;; pixel-scroll-precision-large-scroll-height 5
            pixel-scroll-precision-use-momentum t)
      ;; (global-set-key [remap mwheel-scroll] 'pixel-scroll-precision)

      (pixel-scroll-precision-mode)))

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

(setup show-paren
  (setq show-paren-style 'parenthesis
        show-paren-context-when-offscreen t)
  (show-paren-mode t))


(setq auth-source-debug 'trivia)
;; (auth-source-pass-enable)


(when-let ((exe (executable-find "hunspell")))
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

;; when on a tab, make the cursor the tab length
(setq-default x-stretch-cursor t)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(global-set-key (kbd "C-z") #'undo)

(setq-default fill-column 80)

(add-hook 'before-save-hook #'time-stamp)

(setup hl-line
  (defun my/hl-line-hightlight-unless-region-active
      ()
    (if (region-active-p)
        nil
      (cons (line-beginning-position) (line-beginning-position 2))))
  (setq hl-line-range-function #'my/hl-line-hightlight-unless-region-active)
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode))

(add-hook 'prog-mode-hook (lambda()
                            (setq fill-column 99)))
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)

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
      x-select-enable-primary t  ;; after mouse selection in X11, you can paste by `yank' in emacs
      save-interprogram-paste-before-kill t
      ;; Filename completion ignores these.
      completion-ignored-extensions (append completion-ignored-extensions
                                            '(".pyc" ".o" ".so" ".os" ".cmi" ".cmx" ".rsm" ".rsr"))
      backward-delete-char-untabify-method 'nil	;; don´t untabify, just delete one char
      font-lock-maximum-decoration t			;; maximum decoration
      next-line-add-newlines nil			;; don´t add newlines when trying to move cursor behind eof
      default-indicate-empty-lines t
      line-number-display-limit-width 100000
      kill-whole-line t				;; make kill-line at beginning of line kill the whole line
      woman-use-own-frame nil				;; don't create new frame for manpages
      vc-follow-symlinks t				;; follow symlinks and don't ask
      enable-recursive-minibuffers t)


(defalias 'br 'boxquote-region)
(defalias 'cc 'cider-connect)
(defalias 'sbke 'save-buffers-kill-emacs)
(defalias 'g 'gnus)
(defalias 'ee 'eval-expression)
(defalias 'rb 'revert-buffer)

(defun untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(setq epg-pinentry-mode 'loopback)

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


;; let me tyoe umlauts with right option + u and do not mess with the left option key
(if (boundp 'ns-alternate-modifier)
    (setq ns-alternate-modifier 'meta
          ns-right-alternate-modifier 'none))


;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:1e468b2a-9bee-4571-8454-e3f5462d9321
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)


(provide 'setup-core)
;;; setup-core.el ends here
