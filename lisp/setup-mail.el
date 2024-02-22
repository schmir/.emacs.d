;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

(use-package ebdb
  :after (:any gnus message)
  :init
  (setq ebdb-sources "~/.ebdb"
        ebdb-permanent-ignores-file "~/.ebdb-permanent-ignores")
  :config
  ;; load code for GNUs for reading and message for sending
  (require 'ebdb-gnus)
  (require 'ebdb-message)
  ;; use complete at point interface to select email from contacts
  (setq ebdb-complete-mail 'capf
        ebdb-mua-pop-up nil             ; don't show any pop ups
        ;; when reading or sending with the "reader" in GNUS create contact if it does not exist
        ebdb-gnus-auto-update-p 'query
        ;; save on exit
        ebdb-save-on-exit t))

(use-builtin emacs
  :init
  (setq send-mail-function 'message-send-mail-with-sendmail
        message-send-mail-function 'message-send-mail-with-sendmail
        mail-specify-envelope-from t
        mail-envelope-from 'header
        message-sendmail-envelope-from 'header
        gnus-init-file (expand-file-name "~/.gnus-init.el"))

  ;; we substitute sendmail with msmtp if it's installed
  (let ((msmtp (executable-find "msmtp")))
    (when msmtp
      (setq sendmail-program msmtp))))

(provide 'setup-mail)
