
(setq abbrev-file-name (concat dotfiles-dir "abbrevs_defs")
      save-abbrevs t
      default-abbrev-mode t)

(abbrev-mode 1)

(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

(provide 'setup-abbrev)
