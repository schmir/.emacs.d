(mouse-wheel-mode 1)
;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'exile)
(setq mouse-avoidance-threshold 10
      mouse-avoidance-nudge-dist 20
      mouse-avoidance-nudge-var 5)

(global-set-key [mouse-3] 'imenu)
(global-set-key (kbd "<mouse-3>") 'mouse-buffer-menu)
(provide 'setup-mouse)
