(require 'sequential-command)
(define-sequential-command my-home
  back-to-indentation
  ;; beginning-of-line
  beginning-of-buffer
  seq-return)

(define-sequential-command my-end
  end-of-line
  end-of-buffer
  seq-return)

(provide 'setup-sequential-command)
