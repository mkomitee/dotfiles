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

(req-package expand-region
  :config (progn
            (define-key evil-normal-state-map "]r" 'er/expand-region)
            (define-key evil-visual-state-map "]r" 'er/expand-region)
            (define-key evil-visual-state-map "[r" 'er/contract-region)
            )
  )

(define-key evil-window-map (kbd "<left>") 'winner-undo)
(define-key evil-window-map (kbd "<right>") 'winner-redo)

(define-key evil-normal-state-map (kbd "<down>") 'shrink-window)
(define-key evil-normal-state-map (kbd "<up>") 'enlarge-window)
(define-key evil-normal-state-map (kbd "<right>") 'enlarge-window-horizontally)
(define-key evil-normal-state-map (kbd "<left>") 'shrink-window-horizontally)

;; This makes those windows with lists of possible commands more useful
(req-package guide-key
  :require popwin
  :diminish guide-key-mode
  :config (progn
            (setq guide-key/guide-key-sequence '("C-x" "C-c" "SPC" "M-g" "M-s"
                                                 "z" "g" "]" "[" "Z" "C-w")
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
              "s" 'sort-lines
              "u" 'universal-argument
              ;; I map C-h to evil-window-left, which kills my help-map prefix. I
              ;; replicate most of its functionality here.
              "hf" 'describe-function
              "hk" 'describe-key
              "hv" 'describe-variable
              "hs" 'describe-syntax
              "hP" 'describe-package
              "hm" 'describe-mode
              "hd" 'apropos-documentation
              "hb" 'describe-bindings
              "ha" 'apropos-command
              "hc" 'describe-key-briefly
              "hS" 'info-lookup-symbol
              "hL" 'describe-language-environment
              "hC" 'describe-coding-system
              "hI" 'describe-input-method
              "h?" 'help-for-help
              "h." 'display-local-help
              "cv" 'customize-variable
              "cg" 'customize-group
              "ct" 'customize-themes
              "cm" 'customize-mode
              "cf" 'customize-face
              "ca" 'customize-apropos
              )
            )
  )

;; Easier window navigation. Note, this kills the C-h help-map prefix,
;; which is why I replicate most of that functionality in my leader-map.
(define-key global-map "\C-j" 'evil-window-down)
(define-key global-map "\C-k" 'evil-window-up)
(define-key global-map "\C-h" 'evil-window-left)
(define-key global-map "\C-l" 'evil-window-right)

(define-key evil-normal-state-map "|" 'evil-window-vsplit)
(define-key evil-normal-state-map "_" 'evil-window-split)

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
            ;; bind evil-args text objects
            (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
            (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

            ;; bind evil-forward/backward-args
            (define-key evil-normal-state-map "L" 'evil-forward-arg)
            (define-key evil-normal-state-map "H" 'evil-backward-arg)
            (define-key evil-motion-state-map "L" 'evil-forward-arg)
            (define-key evil-motion-state-map "H" 'evil-backward-arg)

            ;; bind evil-jump-out-args
            (define-key evil-normal-state-map "K" 'evil-jump-out-args)
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

;; escape quits
(define-key evil-normal-state-map [escape] 'komitee/nohl-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
