(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(when (boundp 'package-archive-priorities)
  (setq package-archive-priorities
	'(("melpa-stable" . 20)
	  ("melpa" . 15)
	  ("gnu" . 10))))

(package-initialize)

(defvar my-package-list
  '(use-package pkg-info
     smartparens boxquote diminish
     framemove rainbow-mode whole-line-or-region
     textile-mode yaml-mode
     smart-tab gist w3m flycheck
     nginx-mode batch-mode bbdb)
  "A list of packages to ensure are installed at launch.")

; fetch the list of packages available
(unless package-archive-contents
  (message "refreshing package database...")
  (package-refresh-contents))

; install the missing packages
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'diminish)

(provide 'setup-package)
