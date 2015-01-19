
;; automatically chmod +x when the file has shebang "#!"
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; enable smerge mode
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

(provide 'setup-file-hook)
