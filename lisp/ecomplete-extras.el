;;; ecomplete-extras.el --- A few extra function for ecomplete  -*- lexical-binding: t; -*-
;;; Copied from https://raw.githubusercontent.com/oantolin/emacs-config/refs/heads/master/my-lisp/ecomplete-extras.el

(require 'ecomplete)

(defun email--name+address (email)
  "Return a pair of the name and address for an EMAIL."
  (let (name)
    (when (string-match "^\\(?:\\(.*\\) \\)?<\\(.*\\)>$" email)
      (setq name (match-string 1 email)
            email (match-string 2 email)))
    (cons name email)))

(defun ecomplete--read-address ()
  "Read an email address with completion."
  (unless ecomplete-database (ecomplete-setup))
  (completing-read "Email address: " (ecomplete-completion-table 'mail)))

(defun add-email-to-ecomplete (email)
  "Add email address to ecomplete's database."
  (interactive "sEmail address: ")
  (pcase-let ((`(,name . ,email) (email--name+address email)))
    (unless name (setq name (read-string "Name: ")))
    (ecomplete-add-item
     'mail email
     (format (cond ((equal name "") "%s%s")
                   ((string-match-p "^\\(?:[A-Za-z0-9 ]*\\|\".*\"\\)$" name)
                    "%s <%s>")
                   (t "\"%s\" <%s>"))
             name email))
    (ecomplete-save)))

(defun remove-email-from-ecomplete (email)
  "Remove email address from ecomplete's database."
  (interactive (list (ecomplete--read-address)))
  (when-let ((email (cdr (email--name+address email))) 
             (entry (ecomplete-get-item 'mail email)))
    (setf (cdr (assq 'mail ecomplete-database))
          (remove entry (cdr (assq 'mail ecomplete-database))))
    (ecomplete-save)))

(defun compose-mail-to (address)
  "Compose email to ADDRESS from ecomplete's database."
  (interactive (list (ecomplete--read-address)))
  (compose-mail address))

(provide 'ecomplete-extras)
