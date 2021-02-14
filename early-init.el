;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

(defconst my/start-time (current-time))
(add-hook 'emacs-startup-hook
          `(lambda ()
             (garbage-collect)
             (message "Load time %.06f"
                      (float-time (time-since my/start-time)))) t)
(setq comp-deferred-compilation t)
(setq load-prefer-newer noninteractive)

;; increase some internal limits related to elisp execution
(setq load-prefer-newer t
      gc-cons-threshold most-positive-fixnum  ;; will be reverted by gcmh package
      max-specpdl-size 5000
      max-lisp-eval-depth 6000)

(setq package-enable-at-startup nil)

;; get rid of visual clutter
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; bootstrap straight.el
;; https://github.com/raxod502/straight.el/blob/develop/README.md#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(straight-use-package 'modus-operandi-theme)
(load-theme 'modus-operandi t)

(provide 'early-init)
