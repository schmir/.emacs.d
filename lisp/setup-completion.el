;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

(setup (:package company)
  (:option company-idle-delay 0.8
           company-minimum-prefix-length 0
           company-tooltip-align-annotations t
           company-tooltip-flip-when-above t
           company-show-numbers t))

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

;; Configure Tempel
(setup (:package tempel)
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


(provide 'setup-completion)
;;; setup-completion.el ends here
