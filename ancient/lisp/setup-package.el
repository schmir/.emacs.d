(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(setq package-archive-priorities
      '(("melpa-stable" . 20)
	("melpa" . 15)
	("gnu" . 10)))

(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (message "refreshing package database...")
  (package-refresh-contents))

(defun schmir-ensure-package (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(schmir-ensure-package 'use-package)

(defvar my-package-list
  '(use-package pkg-info
     smartparens boxquote diminish
     framemove rainbow-mode whole-line-or-region
     textile-mode yaml-mode
     smart-tab gist w3m flycheck
     nginx-mode batch-mode bbdb)
  "A list of packages to ensure are installed at launch.")


; install the missing packages
(dolist (package my-package-list)
  (schmir-ensure-package package)
  )

(require 'diminish)

(provide 'setup-package)
