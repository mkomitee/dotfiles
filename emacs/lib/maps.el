
;; Like C-p in vim
(define-key global-map (kbd "C-c b") 'switch-to-buffer)
(define-key global-map (kbd "C-;") 'evilnc-comment-or-uncomment-lines)


;; Allows completion for commands.
(define-key global-map (kbd "M-x") 'smex)
(define-key global-map (kbd "C-x C-m") 'smex)
(define-key global-map (kbd "C-c C-m") 'smex)
(define-key global-map (kbd "C-c /") 'ag-regexp-project-at-point)

;; This is like w and f in easy-motion in vim.
(define-key global-map (kbd "C-c w") 'ace-jump-word-mode)
(define-key global-map (kbd "C-c c") 'ace-jump-char-mode)
(define-key global-map (kbd "C-c l") 'ace-jump-line-mode)
(define-key global-map (kbd "C-c f") 'ace-jump-char-mode)

(define-key global-map (kbd "C-c |") 'split-window-horizontally)
(define-key global-map (kbd "C-c _") 'split-window-vertically)

(define-key global-map (kbd "S-M-<left>") 'windmove-left)
(define-key global-map (kbd "S-M-<right>") 'windmove-right)
(define-key global-map (kbd "S-M-<down>") 'windmove-down)
(define-key global-map (kbd "S-M-<up>") 'windmove-up)
(define-key global-map (kbd "C-c C-k") 'delete-window)

(define-key global-map (kbd "M-p") 'ace-window)

;; http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
(define-key global-map (kbd "C-x C-k") 'delete-window)
(define-key global-map (kbd "M-o") 'other-window)
(define-key global-map (kbd "S-C-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "S-C-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "S-C-<down>") 'shrink-window)
(define-key global-map (kbd "S-C-<up>") 'enlarge-window)

;; http://www.masteringemacs.org/articles/2010/10/04/beginners-guide-to-emacs/
(define-key global-map (kbd "RET") 'newline-and-indent)

;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

(define-key global-map (kbd "C-'") 'bookmark-set)
(define-key global-map (kbd "M-'") 'bookmark-jump)

(define-key global-map (kbd "C-`") 'push-mark-no-activate)
(define-key global-map (kbd "M-`") 'jump-to-mark)

(define-key global-map (kbd "C-%") 'goto-match-paren)

;; This makes those windows with lists of possible commands more useful
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "SPC" "M-g" "M-s" "z" "g" "]" "[" "Z"))
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
  "ev" (lambda () (interactive) (find-file "~/.dotfiles/emacs/init.el"))
  "cc" 'evilnc-comment-or-uncomment-lines
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
(define-key evil-normal-state-map "|" (kbd ":vsplit C-m C-l"))
(define-key evil-normal-state-map "_" (kbd ":split C-m C-j"))
(define-key evil-normal-state-map "j" (kbd "gj"))
(define-key evil-normal-state-map "k" (kbd "gk"))
(define-key evil-normal-state-map "Y" (kbd "y$"))

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

;; indent god damnit.
(define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)

;; eshell is ... different.
(evil-define-key 'insert eshell-mode-map (kbd "RET") 'eshell-send-input)


;; escape quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
