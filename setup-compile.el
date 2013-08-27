(require 'eproject)
(define-project-type generic-hg (generic) (look-for ".hg/00changelog.i")
  :irrelevant-files ("^[.]" "^[#]" ".hg/"))

(defun examine-project ()
  (interactive)
  (message "examine project %s" (eproject-root))
  (if (file-exists-p (concat (eproject-root) "SConstruct"))
      (setq compile-command (concat "cd " (eproject-root) "; scons")))
  (if (file-exists-p (concat (eproject-root) "Makefile"))
      (setq compile-command "make")))


(add-hook 'generic-git-project-file-visit-hook 'examine-project)
(add-hook 'generic-hg-project-file-visit-hook 'examine-project)

(when (require-try 'compile-dwim)
  (global-set-key (quote [f9]) 'compile-dwim)
  (global-set-key "c" 'compile-dwim))
(defadvice yes-or-no-p (around no-query-compilation-always-kill activate)
  "make `compile' always kill existing compilation."
  (if (string-match "A compilation process is running; kill it\\?"
		     prompt)
      (setq ad-return-value t)
    ad-do-it))

(setq compilation-ask-about-save nil)

(defun my-set-compile-command()
  "If first line contains #!/usr/... set compile-command to the file itself"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (and (looking-at "#! */")
	     (file-executable-p buffer-file-name)
	     (not (eq (variable-binding-locus 'compile-command) (current-buffer))))
	(progn
	  (make-local-variable 'compile-command)
	  (setq compile-command buffer-file-name)))))

(add-hook 'find-file-hook 'my-set-compile-command)
(provide 'setup-compile)
