(use-package selectrum :disabled :demand t
  :config
  (progn
    (selectrum-mode +1)))

(use-package selectrum-prescient :disabled :demand t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package hotfuzz :disabled
  :init
  (setq completion-styles '(hotfuzz)))

;; orderless completion style interferes with cider's completion
;; see https://github.com/clojure-emacs/cider/issues/3019
(use-package orderless :disabled
  :init
  (setq completion-styles '(orderless)))


(use-package ctrlf :disabled :demand t
  :config
  (ctrlf-mode +1))

(use-package lsp-mode
  :disabled true
  :init
  (setq schmir/lsp-settings nil)  ;; this is meant to be set via .dir-locals.el
  (setq lsp-keymap-prefix "C-c l"
        lsp-eldoc-render-all t
        lsp-before-save-edits t
        lsp-enable-imenu t
        lsp-idle-delay 0.1
        lsp-headerline-breadcrumb-enable nil)
  (add-hook
   'hack-local-variables-hook
   (lambda ()
     (when (derived-mode-p 'go-mode)
       (require 'lsp)
       (message "lsp-register-custom-settings: %s" schmir/lsp-settings)
       (lsp-register-custom-settings schmir/lsp-settings)
       (lsp))))
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("gopls.gofumpt" t t)))
  (setq lsp-enable-file-watchers nil)
  (defun hide-lsp-no-formatting-changes (func &rest r)
    (unless (string-prefix-p "No formatting changes" (car r))
      (apply func r)))

  (advice-add 'lsp--info :around #'hide-lsp-no-formatting-changes)

  :bind (:map lsp-mode-map
              ("C-c ." . #'lsp-find-references)
              ("C-c t" . #'lsp-find-type-definition)
              ("C-c i" . #'lsp-find-implementation)
              ("C-c r" . #'lsp-rename))
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
