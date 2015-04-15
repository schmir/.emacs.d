(require 'tramp)

(add-to-list 'tramp-default-method-alist
	     '("\\`localhost\\'" "\\`root\\'" "ssh"))

(if (boundp 'tramp-remote-path)
    (progn
      (add-to-list 'tramp-remote-path "~/bin")
      (add-to-list 'tramp-remote-path "~/local/bin")
      (add-to-list 'tramp-remote-path "~/rc/bin")))

(setq tramp-default-method "ssh"
      tramp-ssh-controlmaster-options "")

(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)

(provide 'setup-tramp)
