(setq ps-lpr-command "gtklp"
      ps-lpr-switches '("-X")
      ps-landscape-mode nil
      ps-number-of-columns 1
      ps-n-up-printing 2
      ps-n-up-border-p nil
      ps-line-number t
      ps-line-number-step 'zebra
      ps-left-margin 14
      ps-right-margin 14
      ps-top-margin 14
      ps-bottom-margin 14
      ps-zebra-stripes t
      ps-zebra-stripe-height 10
      ps-zebra-color 0.98
      ps-paper-type 'a4)

(provide 'setup-print)
