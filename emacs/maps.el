;; http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
(define-key global-map (kbd "S-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "S-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "S-<down>") 'shrink-window)
(define-key global-map (kbd "S-<up>") 'enlarge-window)


(req-package expand-region
  :require evil
  :config (progn
            (define-key evil-motion-state-map "]r" 'er/expand-region)
            (define-key evil-visual-state-map "]r" 'er/expand-region)
            (define-key evil-visual-state-map "[r" 'er/contract-region)
            )
  )


;; This makes those windows with lists of possible commands more useful
(req-package guide-key
  :require popwin
  :diminish guide-key-mode
  :config (progn
            (setq guide-key/guide-key-sequence '("C-x" "C-c" "SPC" "M-g" "M-s"
                                                 "z" "g" "]" "[" "Z" "C-w"
                                                 "M-SPC")
                  guide-key/recursive-key-sequence-flag t
                  guide-key/idle-delay 0.5
                  guide-key/popup-window-position (quote bottom)
                  )
            (guide-key-mode 1)
            )
  )

(req-package evil-leader
  :require evil
  :config (progn
            (evil-leader/set-leader "<SPC>")
            (global-evil-leader-mode)
            (evil-leader/set-key
              "u" 'universal-argument

              "hdp" 'describe-package
              "hdcs" 'describe-coding-system
              "hdf" 'describe-function
              "hdb" 'describe-bindings
              "hdim" 'describe-input-method
              "hdk" 'describe-key
              "hdle" 'describe-language-environment
              "hdM" 'describe-mode
              "hdm" 'describe-minor-mode
              "hds" 'describe-syntax
              "hdv" 'describe-variable

              "hac" 'apropos-command
              "had" 'apropos-documentation
              "hdlh" 'display-local-help
              "hfh" 'help-for-help
              "hils" 'info-lookup-symbol

              "hca" 'customize-apropos
              "hcf" 'customize-face
              "hcg" 'customize-group
              "hcm" 'customize-mode
              "hct" 'customize-themes
              "hcv" 'customize-variable

              "o" 'delete-other-windows

              "w" 'evil-window-map
              )
            )
  )

(req-package evil-args
  :require evil
  :config (progn
            (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
            (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
            (define-key evil-motion-state-map (kbd "M-l") 'evil-forward-arg)
            (define-key evil-motion-state-map (kbd "M-h") 'evil-backward-arg)
            (define-key evil-motion-state-map (kbd "M-k") 'evil-jump-out-args)
            )
  )

