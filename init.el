;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;; on lexical-binding: https://nullprogram.com/blog/2016/12/22/

;; focus initial frame, need this for macos
(if (eq 'ns (window-system))
    (x-focus-frame nil))
(add-hook 'after-make-frame-functions #'x-focus-frame)

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "init.el: Emacs too old -- this config requires at least v%s" minver)))

;;; Initialize lisp directory
(progn
  (setopt site-lisp-directory (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))
  (add-to-list 'load-path site-lisp-directory)
  (require 'setup-lisp-directory)
  (setup-lisp-directory))

(require 'setup-setup)
(require 'setup-ui)
(require 'setup-core)
(require 'setup-editing)
(require 'setup-languages)
(require 'setup-misc)
(require 'setup-mail)
(require 'setup-completion)
(require 'setup-git)
(require 'setup-lisp)
(require 'setup-go)
(require 'setup-python)
(require 'setup-shell)


;; some aliases for interactive use with M-x
(defalias 'br 'boxquote-region)
(defalias 'cc 'cider-connect)
(defalias 'sbke 'save-buffers-kill-emacs)
(defalias 'g 'gnus)
(defalias 'ee 'eval-expression)
(defalias 'rb 'revert-buffer)

;;; init.el ends here
