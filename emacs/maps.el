;; Allows completion for commands.
(define-key global-map (kbd "M-x") 'smex)
(define-key global-map (kbd "C-x C-m") 'smex)
(define-key global-map (kbd "C-c C-m") 'smex)
(define-key global-map (kbd "C-c /") 'ag-regexp-project-at-point)

;; http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
(define-key global-map (kbd "S-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "S-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "S-<down>") 'shrink-window)
(define-key global-map (kbd "S-<up>") 'enlarge-window)

;; Not sure about these.
(define-key global-map (kbd "C-'") 'bookmark-set)
(define-key global-map (kbd "M-'") 'bookmark-jump)
(define-key global-map (kbd "C-`") 'push-mark-no-activate)
(define-key global-map (kbd "M-`") 'jump-to-mark)

(define-key evil-normal-state-map (kbd "[b") 'evil-prev-buffer)
(define-key evil-normal-state-map (kbd "]b") 'evil-next-buffer)
(define-key evil-normal-state-map (kbd "[w") 'evil-window-prev)
(define-key evil-normal-state-map (kbd "]w") 'evil-window-next)
(define-key evil-normal-state-map (kbd "[e") 'previous-error)
(define-key evil-normal-state-map (kbd "]e") 'next-error)
(define-key evil-normal-state-map (kbd "]s") 'flyspell-goto-next-error)
(define-key evil-normal-state-map (kbd "C-w ?") 'ace-window)
(define-key evil-normal-state-map (kbd "C-w <left>") 'winner-undo)
(define-key evil-normal-state-map (kbd "C-w <right>") 'winner-redo)
(define-key evil-normal-state-map (kbd "<down>") 'shrink-window)
(define-key evil-normal-state-map (kbd "<up>") 'enlarge-window)
(define-key evil-normal-state-map (kbd "<right>") 'enlarge-window-horizontally)
(define-key evil-normal-state-map (kbd "<left>") 'shrink-window-horizontally)

;; vim style auto-complete bindings.
(define-key evil-insert-state-map (kbd "C-x C-o") 'company-complete)
(define-key evil-insert-state-map (kbd "C-x C-u") 'company-complete)

;; This makes those windows with lists of possible commands more useful
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "SPC" "M-g" "M-s" "z" "g" "]" "[" "Z" "C-w"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)

(evil-leader/set-key
  "b" 'switch-to-buffer
  "p" 'projectile-find-file
  "r" 'projectile-recentf
  "a" 'projectile-ag
  "/" 'projectile-ag
  "w" 'evil-ace-jump-word-mode
  "f" 'evil-ace-jump-char-mode
  "t" 'evil-ace-jump-char-to-mode
  "j" 'evil-ace-jump-line-mode
  "'" 'ace-jump-mode-pop-mark
  "`" 'ace-jump-mode-pop-mark
  "ev" (lambda () (interactive) (dired komitee/emacs-config-directory))
  "c" 'evilnc-comment-operator
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
  "h." 'display-local-help)

;; Easier window navigation. Note, this kills the C-h help-map prefix,
;; which is why I replicate most of that functionality in my leader-map.
(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)
(define-key evil-normal-state-map "|" 'evil-window-vsplit)
(define-key evil-normal-state-map "_" 'evil-window-split)
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)
(define-key evil-operator-state-map "j" 'evil-next-visual-line)
(define-key evil-operator-state-map "k" 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "0") 'smarter-move-beginning-of-line)
(define-key evil-visual-state-map (kbd "0") 'smarter-move-beginning-of-line)
(define-key evil-operator-state-map (kbd "0") 'smarter-move-beginning-of-line)
(define-key evil-normal-state-map "Y" (kbd "y$"))
(define-key evil-normal-state-map "+" 'er/expand-region)
(define-key evil-visual-state-map "+" 'er/expand-region)
(define-key evil-visual-state-map "-" 'er/contract-region)

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
(evil-ex-define-cmd "ag" 'projectile-ag)

;; indent god damnit.
(define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)

;; eshell is ... different.
(evil-define-key 'insert eshell-mode-map (kbd "RET") 'eshell-send-input)

;; escape quits
(define-key evil-normal-state-map [escape] 'komitee/nohl-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
