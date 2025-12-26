;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

;; company: Text completion framework (used for some modes)
(setup (:package company)
  (:option company-idle-delay 0.8
           company-minimum-prefix-length 0
           company-tooltip-align-annotations t
           company-tooltip-flip-when-above t
           company-show-numbers t))

;; corfu: Popup completion UI at point
(setup (:package corfu)
  (global-corfu-mode)

  ;; Optional customizations
  (:option
   corfu-cycle t                ;; Enable cycling for `corfu-next/previous'
   corfu-auto t                 ;; Enable auto completion
   corfu-auto-delay 0.2
   corfu-auto-prefix 1
   ;; (corfu-separator ?\s)          ;; Orderless field separator
   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
   corfu-preselect 'prompt      ;; Preselect the prompt
   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

   ;; Enable Corfu only for certain modes.
   ;; :hook ((prog-mode . corfu-mode)
   ;;        (shell-mode . corfu-mode)
   ;;        (eshell-mode . corfu-mode))

   ;; Recommended: Enable Corfu globally.
   ;; This is recommended since Dabbrev can be used globally (M-/).
   ;; See also `global-corfu-modes'.
   ))

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

;; orderless: Space-separated completion matching
(setup (:package orderless))

;; vertico: Vertical completion UI in minibuffer
(setup (:package vertico)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)
  (setq completion-styles '(basic substring))
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  (vertico-mode))

;; marginalia: Rich annotations in minibuffer completions
(setup (:package marginalia)
  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  (marginalia-mode +1)
  (keymap-global-set "<remap> <switch-to-buffer>" #'consult-buffer))

;; consult: Enhanced search and navigation commands
(setup (:package consult)
  (:package consult-project-extra)
  (keymap-global-set "<remap> <project-find-file>" #'consult-project-extra-find)
  (with-eval-after-load 'consult
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep consult-buffer
     consult-bookmark consult-recent-file consult-xref
     ;; consult--source-file
     ;; consult--source-project-file
     consult--source-bookmark
     :preview-key "M-.")))

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

(provide 'setup-completion)
;;; setup-completion.el ends here
