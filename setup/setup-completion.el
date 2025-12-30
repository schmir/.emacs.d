;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

;; corfu: Popup completion UI at point
(setup (:package corfu)  
  (setopt tab-always-indent 'complete
          tab-first-completion nil
          corfu-cycle nil
          corfu-preselect 'first
          corfu-on-exact-match 'show
          corfu-scroll-margin 2
          corfu-quit-at-boundary nil
          corfu-preview-current t)
  (add-hook 'after-init-hook #'global-corfu-mode))

;; completion-preview: show one completion candidate
(setup (:and (fboundp #'completion-preview-mode)
             completion-preview)
  (with-eval-after-load 'completion-preview
    (keymap-unset completion-preview-active-mode-map "<TAB>" t)
    (keymap-set completion-preview-active-mode-map "<right>" #'completion-preview-insert))
  (add-hook 'prog-mode-hook #'completion-preview-mode)
  (add-hook 'text-mode-hook #'completion-preview-mode))

;; tempel: Modern template/snippet system
(setup (:package tempel)
  (setopt tempel-path (no-littering-expand-etc-file-name "tempel-templates.eld"))
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")


  (with-eval-after-load 'tempel
    (keymap-set tempel-map "S-<left>"  #'tempel-previous)
    (keymap-set tempel-map "S-<right>" #'tempel-next))

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  ;;(add-hook 'conf-mode-hook 'tempel-setup-capf)
  ;;(add-hook 'prog-mode-hook 'tempel-setup-capf)
  ;;(add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )


;; cape: Completion at point extensions for eglot and dabbrev
(setup (:package cape)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list (cape-capf-super
                                 #'eglot-completion-at-point
                                 #'tempel-complete)
                                t))))
  (with-eval-after-load 'cape
    (add-to-list 'completion-at-point-functions
                 (cape-capf-super
                  #'cape-file
                  (cape-capf-prefix-length #'cape-dabbrev 3)))))

;; hippie-expand: Extensible text expansion with C-<tab>
(setup hippie-expand
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

  (keymap-global-set "C-<tab>" #'hippie-expand))


;; ignore case when completing
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; vertico: Vertical completion UI in minibuffer
;; orderless: Space-separated completion matching
(setup (:package vertico vertico-prescient prescient orderless)
  ;; Show more candidates
  (:option vertico-prescient-enable-sorting t
           vertico-prescient-override-sorting nil
           vertico-prescient-enable-filtering nil)
  (setq vertico-count 20)
  (setq completion-styles '(orderless basic substring))

  (with-eval-after-load 'prescient
    (prescient-persist-mode 1))

  (with-eval-after-load 'vertico
    (vertico-prescient-mode 1)
    ;; Replace `vertico-insert' to enable TAB prefix expansion.
    (keymap-set vertico-map "TAB" #'minibuffer-complete))
  (add-hook 'after-init-hook #'vertico-mode))

;; marginalia: Rich annotations in minibuffer completions
(setup (:package marginalia)
  (marginalia-mode +1))

;; consult: Enhanced search and navigation commands
(setup (:package consult consult-project-extra)
  (keymap-global-set "<remap> <project-find-file>" #'consult-project-extra-find)
  (keymap-global-set "<remap> <switch-to-buffer>" #'consult-buffer)

  (with-eval-after-load 'consult
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep consult-man
     consult-bookmark consult-recent-file consult-xref
     consult-source-bookmark consult-source-file-register
     consult-source-recent-file consult-source-project-recent-file
     consult-theme
     :preview-key '(:debounce 0.5 any))))

;; consult-dir: Quick directory switching with zoxide integration
(setup (:package consult-dir)
  (keymap-global-set "C-x C-d" #'consult-dir)
  (with-eval-after-load 'vertico
    (keymap-set vertico-map "C-x C-d" #'consult-dir)
    (keymap-set vertico-map "C-x C-j" #'consult-dir-jump-file))

  (defvar my/consult-dir-source-zoxide
    `(:name "Zoxide"
            :narrow ?z
            :category file
            :face consult-file
            :enabled ,(lambda () (fboundp #'zoxide-query))
            :items ,(lambda()
                      (delete-dups (mapcar #'abbreviate-file-name (zoxide-query)))))
    "Zoxide directory source for `consult-dir--pick'.")

  (with-eval-after-load 'consult-dir
    (add-to-list 'consult-dir-sources #'my/consult-dir-source-zoxide)))

(setup (:package embark embark-consult)
  (keymap-global-set "C-." #'embark-act)
  (add-hook 'context-menu-functions #'embark-context-menu 100)
  ;; (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

  (with-eval-after-load 'embark
    (keymap-set embark-symbol-map "<remap> <describe-symbol>" #'helpful-symbol)
    (keymap-set embark-variable-map "<remap> <describe-symbol>" #'helpful-variable)))

(provide 'setup-completion)
;;; setup-completion.el ends here
