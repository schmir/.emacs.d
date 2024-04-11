;;; cwc.el --- whitespace-cleanup only for changed lines -*- lexical-binding: t -*-
;;
;; Author: Ralf Schmitt <ralf@systemexit.de>
;; Time-stamp: <2024-04-11 12:38:26 ralf>
;; Version: 0.2

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
;;     (cwc-global-mode)
;;
;;; Code:

(require 'whitespace)

;;;###autoload
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

(defun cwc-clear-changes()
  (interactive)
  (if (highlight-changes-mode)
      (progn
	(message "clearing changes")
	(highlight-changes-mode -1)
	(highlight-changes-mode 1))))

(defvar cwc-lighter " CWC")

(defvar cwc-mode)

;; (setq highlight-changes-visibility-initial-state nil)
;; (add-to-list 'whitespace-style 'trailing)

;;;###autoload
(define-minor-mode cwc-mode
  "Minor mode for cleaning up whitespace only on changed lines dfg"
  :lighter cwc-lighter
  (if cwc-mode
      (progn
        (highlight-changes-mode +1)
        (highlight-changes-visible-mode -1)
        (add-hook 'before-save-hook #'changed-whitespace-cleanup nil 'local)
        (add-hook 'after-revert-hook #'cwc-clear-changes nil 'local))
    (highlight-changes-mode -1)
    (remove-hook 'before-save-hook #'changed-whitespace-cleanup 'local)
    (remove-hook 'after-revert-hook #'cwc-clear-changes 'local)))

;;;###autoload
(define-globalized-minor-mode cwc-global-mode
  cwc-mode cwc-mode)

(provide 'cwc)
;;; cwc.el ends here
