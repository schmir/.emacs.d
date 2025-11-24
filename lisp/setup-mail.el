;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

(setup emacs
  (setq send-mail-function 'message-send-mail-with-sendmail
        message-send-mail-function 'message-send-mail-with-sendmail
        mail-specify-envelope-from t
        mail-envelope-from 'header
        message-sendmail-envelope-from 'header
        gnus-init-file (expand-file-name "~/.gnus-init.el"))

  ;; we substitute sendmail with msmtp if it's installed
  (when-let* ((msmtp (executable-find "msmtp")))
    (setq sendmail-program msmtp))

  ;; ecomplete setup, see
  ;; https://www.reddit.com/r/emacs/comments/sl33w6/ecomplete_the_emacs_contact_manager_you_were/
  (require 'ecomplete)
  (require 'ecomplete-extras)
  (setq ecomplete-database-file (expand-file-name "~/.config/ecomplete/ecomplete-database.el"))
  (ecomplete-setup)
  (setq message-mail-alias-type 'ecomplete
        message-self-insert-commands nil
        message-expand-name-standard-ui t)

  (add-hook 'message-sent-hook 'message-put-addresses-in-ecomplete)
  ;; end ecomplete setup

  )

(when-let* ((viewer (cond
                     ((eq system-type 'darwin)
                      "open %s 2>/dev/null")
                     ((eq system-type 'gnu/linux)
                      "xdg-open %s 2>/dev/null"))))
  (message "Using '%s' as external PDF viewer" viewer)
  (push `((viewer . ,viewer)
          (type . "application/pdf")
          (test . window-system))
        mailcap-user-mime-data))

(provide 'setup-mail)
