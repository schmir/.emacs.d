(require 'highlight-symbol)
(diminish 'highlight-symbol-mode)

(setq highlight-symbol-idle-delay 0.3)

(global-set-key [(control f1)] 'highlight-symbol-at-point)
(global-set-key [f1] 'highlight-symbol-next)
(global-set-key [(shift f1)] 'highlight-symbol-prev)
(global-set-key [(meta f1)] 'highlight-symbol-query-replace)

(provide 'setup-highlight-symbol)
