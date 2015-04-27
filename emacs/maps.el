;; http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
(define-key global-map (kbd "S-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "S-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "S-<down>") 'shrink-window)
(define-key global-map (kbd "S-<up>") 'enlarge-window)


(req-package expand-region
  :require evil
  :config (evil-define-key 'motion global-map
            "]r" 'er/expand-region
            "[r" 'er/contract-region
            )
  )


;; This makes those windows with lists of possible commands more useful
(req-package guide-key
  :require popwin
  :diminish guide-key-mode
  :config (progn
            (setq guide-key/guide-key-sequence '("C-x" "C-c" "SPC" "M-g" "M-s"
                                                 "z" "g" "]" "[" "Z" "C-w"
                                                 "M-SPC" "M-h")
                  guide-key/recursive-key-sequence-flag t
                  guide-key/idle-delay 0.5
                  guide-key/popup-window-position (quote right)
                  )
            (guide-key-mode 1)
            )
  )


