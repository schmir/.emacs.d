;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;; on lexical-binding: https://nullprogram.com/blog/2016/12/22/

;; focus initial frame, need this for macos
(if (eq 'ns (window-system))
    (x-focus-frame nil))
(add-hook 'after-make-frame-functions #'x-focus-frame)

(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

(require 'setup-setup)
(require 'setup-ui)
(require 'setup-core)
(require 'setup-dired)
(require 'setup-editing)
(require 'setup-languages)
(require 'setup-misc)
(require 'setup-mail)
(require 'setup-completion)
(require 'setup-git)
(require 'setup-lisp)
(require 'setup-shell)
(require 'language-profiles)
(my/language-profiles-install)


;; some aliases for interactive use with M-x
(defalias 'br 'boxquote-region)
(defalias 'cc 'cider-connect)
(defalias 'sbke 'save-buffers-kill-emacs)
(defalias 'g 'gnus)
(defalias 'ee 'eval-expression)
(defalias 'rb 'revert-buffer)

;;; init.el ends here
