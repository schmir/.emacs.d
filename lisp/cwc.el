;;; cwc.el --- Run whitespace-cleanup only for changed lines -*- lexical-binding: t -*-
;;
;; URL: https://github.com/schmir/.emacs.d/blob/main/lisp/cwc.el
;; Author: Ralf Schmitt <ralf@systemexit.de>
;; Time-stamp: <2024-04-12 07:44:13 ralf>
;; Version: 0.2
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;;
;; Run 'whitespace-cleanup' only on changed lines in a buffer.  Needs
;; highlight-changes mode to do it's work.

;;; Installation
;;
;; Put this file into your load-path and add the following to your
;; Emacs init file in order to cleanup whitespace when saving buffers:
;;
;;     (require 'cwc)
;;     (cwc-global-mode)
;;
;;; Code:

(require 'whitespace)
(require 'hilit-chg)

;;;###autoload
(defun cwc-cleanup()
  "Cleanup whitespace in current buffer.

This function runs `whitespace-cleanup-region' on all changed lines.  The buffer
has to be in `highlight-changes-mode'."
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
    (message "cwc.el: buffer %s is not in highlight-changes-mode" (buffer-name))))


;; (setq show-trailing-whitespace (not show-trailing-whitespace))

(defun cwc-clear-changes()
  "Clear text properties stored by `highlight-changes-mode'."
  (if (highlight-changes-mode)
      (progn
        (message "cwc.el: clearing changes")
	(highlight-changes-mode -1)
	(highlight-changes-mode 1))))

(defvar cwc-lighter " CWC")

(defvar cwc-mode)

;; (setq highlight-changes-visibility-initial-state nil)
;; (add-to-list 'whitespace-style 'trailing)

;;;###autoload
(define-minor-mode cwc-mode
  "Minor mode for cleaning up whitespace only on changed lines."
  :lighter cwc-lighter
  (if cwc-mode
      (progn
        ;; only enable cwc-mode when global-highlight-changes-mode would also turn on
        ;; highlight-changes-mode
        (highlight-changes-mode-turn-on)
        (if highlight-changes-mode
            (progn
              (highlight-changes-mode +1)
              (highlight-changes-visible-mode -1)
              (add-hook 'before-save-hook #'cwc-cleanup nil 'local)
              (add-hook 'after-revert-hook #'cwc-clear-changes nil 'local))
          (progn
            ;;(message "cwc.el: cannot turn on highlight-changes-mode in buffer %s" (buffer-name))
            (setq cwc-mode nil))))
    (highlight-changes-mode -1)
    (remove-hook 'before-save-hook #'cwc-cleanup 'local)
    (remove-hook 'after-revert-hook #'cwc-clear-changes 'local)))

;;;###autoload
(define-globalized-minor-mode cwc-global-mode
  cwc-mode cwc-mode)

(provide 'cwc)
;;; cwc.el ends here
