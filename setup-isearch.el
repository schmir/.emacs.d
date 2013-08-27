(defun schmir-isearch-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))

;; Always end searches at the beginning of the matching expression.
(add-hook 'isearch-mode-end-hook 'schmir-isearch-beginning)
(setq isearch-allow-scroll t)
(define-key isearch-mode-map [next] 'isearch-repeat-forward)
(define-key isearch-mode-map [prior] 'isearch-repeat-backward)
(global-set-key [C-next] 'isearch-repeat-forward)
(global-set-key [C-prior] 'isearch-repeat-backward)

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
	       (regexp-quote isearch-string))))))


;; TAB expands even during isearch (Ctrl-S)
(define-key isearch-mode-map [tab] 'isearch-yank-word)
(setq lazy-highlight-cleanup nil) ;; keep search results highlighted

(provide 'setup-isearch)
