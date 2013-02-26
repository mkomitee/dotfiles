
;; Emulate some of my maps from vim
(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)
(define-key evil-normal-state-map "|" (kbd ":vsplit C-m C-l"))
(define-key evil-normal-state-map "_" (kbd ":split C-m C-j"))
(define-key evil-normal-state-map " b" 'switch-to-buffer)
(define-key evil-normal-state-map " p" 'textmate-goto-file)
(define-key evil-normal-state-map " ev" (kbd ":e ~/.dotfiles/emacs/init.el"))
(define-key evil-normal-state-map " cc" 'comment-or-uncomment-region-or-line)
(define-key evil-visual-state-map " cc" 'comment-or-uncomment-region-or-line)
(define-key evil-visual-state-map " s" 'sort-lines)
(define-key evil-normal-state-map "j" (kbd "gj"))
(define-key evil-normal-state-map "k" (kbd "gk"))

(define-key evil-insert-state-map "\C-j" 'evil-window-down)
(define-key evil-insert-state-map "\C-k" 'evil-window-up)
(define-key evil-insert-state-map "\C-h" 'evil-window-left)
(define-key evil-insert-state-map "\C-l" 'evil-window-right)

;; Since there's no 'noremap' functionality available, I have to first
;; define a sequence of characters to perform the shift, and THEN
;; remap > and < to call that other sequence and then gv, this works
;; but it makes me sad.

(define-key evil-visual-state-map "g>" 'evil-shift-right)
(define-key evil-visual-state-map "g<" 'evil-shift-left)
(define-key evil-visual-state-map ">" (kbd "g>gv"))
(define-key evil-visual-state-map "<" (kbd "g<gv"))

;; I switch ' and ` in vim, so I do so here as well
(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)

;; Arrow keys to resize splits
;; (define-key evil-normal-state-map [up] (kbd "C-w +"))
;; (define-key evil-normal-state-map [down] (kbd "C-w -"))
;; (define-key evil-normal-state-map [left] (kbd "C-w <"))
;; (define-key evil-normal-state-map [right] (kbd "C-w >"))

;; Here's how to define a new ex command
(evil-ex-define-cmd "Q" 'evil-quit)
(evil-ex-define-cmd "QA" 'evil-quit-all)
(evil-ex-define-cmd "Qa" 'evil-quit-all)
(evil-ex-define-cmd "WQ" 'evil-save-and-close)
(evil-ex-define-cmd "Wq" 'evil-save-and-close)
(evil-ex-define-cmd "esh[ell]" 'eshell)
(evil-ex-define-cmd "sort" 'sort-lines)
(evil-ex-define-cmd "python" 'python-shell-switch-to-shell)

(provide 'maps)
