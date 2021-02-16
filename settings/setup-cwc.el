;;; cwc.el --- remove whitespace     -*- lexical-binding: t -*-

;;; Commentary:

(require 'cwc)

;;; Code:

(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)
(add-to-list 'whitespace-style 'trailing)
(add-hook 'before-save-hook 'changed-whitespace-cleanup)

(provide 'setup-cwc)
;;; setup-cwc.el ends here
