;; http://www.emacswiki.org/emacs-ru/WholeLineOrRegion
;; This minor mode allows functions to operate on the current line if
;; they would normally operate on a region and region is currently
;; undefined.

(require 'whole-line-or-region)
(add-to-list 'whole-line-or-region-extensions-alist '(comment-dwim whole-line-or-region-comment-dwim nil))
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(defalias 'whole-line-or-region-kill-region 'schmir-whole-line-or-region-kill-region)

(provide 'setup-whole-line-or-region)
