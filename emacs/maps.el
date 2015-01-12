;; http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
(define-key global-map (kbd "S-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "S-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "S-<down>") 'shrink-window)
(define-key global-map (kbd "S-<up>") 'enlarge-window)

(define-key evil-motion-state-map "[b" 'evil-prev-buffer)
(define-key evil-motion-state-map "]b" 'evil-next-buffer)
(define-key evil-motion-state-map "[w" 'evil-window-prev)
(define-key evil-motion-state-map "]w" 'evil-window-next)
(define-key evil-motion-state-map "[e" 'previous-error)
(define-key evil-motion-state-map "]e" 'next-error)
(define-key evil-motion-state-map "]s" 'flyspell-goto-next-error)

(define-key evil-motion-state-map "]t" 'elscreen-next)
(define-key evil-motion-state-map "[t" 'elscreen-previous)

(req-package expand-region
  :config (progn
            (define-key evil-motion-state-map "]r" 'er/expand-region)
            (define-key evil-visual-state-map "]r" 'er/expand-region)
            (define-key evil-visual-state-map "[r" 'er/contract-region)
            )
  )

(define-key evil-window-map (kbd "<left>") 'winner-undo)
(define-key evil-window-map (kbd "<right>") 'winner-redo)

(define-key evil-motion-state-map (kbd "<down>") 'shrink-window)
(define-key evil-motion-state-map (kbd "<up>") 'enlarge-window)
(define-key evil-motion-state-map (kbd "<right>") 'enlarge-window-horizontally)
(define-key evil-motion-state-map (kbd "<left>") 'shrink-window-horizontally)

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
  :config (progn
            (evil-leader/set-leader "<SPC>")
            (global-evil-leader-mode)
            (evil-leader/set-key
              "b" 'switch-to-buffer
              "ev" (lambda () (interactive)
                     (ido-find-file-in-dir komitee/emacs-config-directory))
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

;; Easier window navigation. Note, this kills the C-h help-map prefix,
;; which is why I replicate most of that functionality in my leader-map.
(define-key global-map "\C-j" 'evil-window-down)
(define-key global-map "\C-k" 'evil-window-up)
(define-key global-map "\C-h" 'evil-window-left)
(define-key global-map "\C-l" 'evil-window-right)

(define-key evil-motion-state-map "|" 'komitee/split-horizontally)
(define-key evil-motion-state-map "_" 'komitee/split-vertically)

(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)

(define-key evil-motion-state-map "0" 'smarter-move-beginning-of-line)
(define-key evil-normal-state-map "Y" (kbd "y$"))

;; There's probably an easier way to do this by defining a function,
;; but I can't figure it out. It re-selects the shifted region after
;; the shift.
(define-key evil-visual-state-map (kbd "C->") 'evil-shift-right)
(define-key evil-visual-state-map ">" (kbd "C-> gv"))
(define-key evil-visual-state-map (kbd "C-<") 'evil-shift-left)
(define-key evil-visual-state-map "<" (kbd "C-< gv"))

(req-package evil-args
  :config (progn
            (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
            (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
            (define-key evil-motion-state-map (kbd "M-l") 'evil-forward-arg)
            (define-key evil-motion-state-map (kbd "M-h") 'evil-backward-arg)
            (define-key evil-motion-state-map (kbd "M-k") 'evil-jump-out-args)
            )
  )

;; I switch ' and ` in vim, so I do so here as well
(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)

;; Here's how to define a new ex command
(evil-ex-define-cmd "Q" 'evil-quit)
(evil-ex-define-cmd "QA" 'evil-quit-all)
(evil-ex-define-cmd "Qa" 'evil-quit-all)
(evil-ex-define-cmd "WQ" 'evil-save-and-close)
(evil-ex-define-cmd "Wq" 'evil-save-and-close)
(evil-ex-define-cmd "esh[ell]" 'eshell)
(evil-ex-define-cmd "sort" 'sort-lines)
(evil-ex-define-cmd "log" 'magit-log)

;; escape quits
(define-key evil-motion-state-map [escape] 'komitee/nohl-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
