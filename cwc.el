;;; cwc.el --- whitespace-cleanup only for changed lines
;;
;; Author: Ralf Schmitt <ralf@systemexit.de>
;; Version: 0.1
;; Last Changed: 2010-04-13 17:03:32 by ralf

;;; Commentary:
;;
;; run whitespace-cleanup only on changed lines in a buffer. needs
;; highlight-changes mode to do it's work.

;;; Installation
;;
;; put this file into your load-path and add the following to your
;; emacs init file in order to cleanup whitespace when saving buffers:
;;
;;     (require 'cwc)
;;     (global-highlight-changes-mode t)
;;     (setq highlight-changes-visibility-initial-state nil)
;;     (add-to-list 'whitespace-style 'trailing)
;;     (add-hook 'before-save-hook 'changed-whitespace-cleanup)
;;
;;; Code:

(require 'whitespace)
(defun changed-whitespace-cleanup()
  "Run whitespace-cleanup-region on all changed lines. The buffer has
to be in highlight-changes-mode."
  (interactive)
  (if highlight-changes-mode
      (save-excursion
	(goto-char (point-min))
	;; strange: why doesn't whitespace.el use indent-tabs-mode and
	;; instead uses whitespace-indent-tabs-mode? the following let
	;; fixes that
	(let ((whitespace-indent-tabs-mode indent-tabs-mode)
	      (whitespace-tab-width tab-width))
	  (catch 'break
	    (let (start end)
	      (while t
		(setq start
		      (if (get-text-property (point) 'hilit-chg)
			  (point)
			(next-single-property-change (point) 'hilit-chg)))
		(unless start (throw 'break nil))

		(goto-char start)
		(setq end (or (next-single-property-change (point) 'hilit-chg) (point-max)))

		;; (message "changed %s-%s // %s" start end (point-max))

		(beginning-of-line)
		(setq start (point))
		(goto-char end)
		(end-of-line)
		(whitespace-cleanup-region start (point))
		(goto-char (+ 1 (point))))))))
    (message "buffer %s is not in highlight-changes-mode" (buffer-name))))


;; (setq show-trailing-whitespace (not show-trailing-whitespace))  
(defun clear-changes()
  (interactive)
  (if (highlight-changes-mode)
      (progn
	(message "clearing changes")
	(highlight-changes-mode -1)
	(highlight-changes-mode 1))))

(add-hook 'after-revert-hook 'clear-changes)

(provide 'cwc)
