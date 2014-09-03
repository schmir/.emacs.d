(require 'key-chord)
(setq key-chord-two-keys-delay 0.05)

(key-chord-mode 1)

;; (key-chord-define-global "yu" 'whole-line-or-region-yank)
;; (key-chord-define-global "yy" 'yank-pop)
;; (key-chord-define-global "ui"     "\M-w") ;; copy
;; (key-chord-define-global "io"     "\C-w") ;; cut

(key-chord-define-global "nm"     'ido-switch-buffer)

(key-chord-define-global "m,"     'ido-find-file)
(key-chord-define-global "./"     'save-buffer)

(key-chord-define-global ":\""    'comment-dwim)
(key-chord-define-global ";\""    'comment-dwim)
(key-chord-define-global ";'"    'comment-dwim)

;; (key-chord-define-global "hj"     'toggle-windows-split)
;; (key-chord-define-global "jk"     'flip-window)
;; (key-chord-define-global "kl"     'other-frame)

(provide 'setup-key-chord)
