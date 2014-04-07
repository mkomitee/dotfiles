
;; Emulate some of my maps from vim
(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)
(define-key evil-normal-state-map "|" (kbd ":vsplit C-m C-l"))
(define-key evil-normal-state-map "_" (kbd ":split C-m C-j"))
(define-key evil-normal-state-map " b" 'helm-buffers-list)
(define-key evil-normal-state-map " p" 'helm-projectile)

(define-key evil-normal-state-map " ev" (kbd ":e ~/.dotfiles/emacs/init.el"))
(define-key evil-normal-state-map " cc" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map " cc" 'evilnc-comment-or-uncomment-lines)

(define-key evil-visual-state-map " s" 'sort-lines)
(define-key evil-normal-state-map "j" (kbd "gj"))
(define-key evil-normal-state-map "k" (kbd "gk"))
(define-key evil-normal-state-map "Y" (kbd "y$"))

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
(evil-ex-define-cmd "Exp[lore]" 'dired-jump)
(evil-ex-define-cmd "color[scheme]" 'customize-themes)

(define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)
(define-key evil-insert-state-map (kbd "RET") 'evil-ret)
(evil-define-key 'insert eshell-mode-map (kbd "RET") 'eshell-send-input)

(after 'ag-autoloads
  (define-key evil-normal-state-map (kbd "SPC /") 'ag-regexp-project-at-point))

;; escape minibuffer
(define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit)

(after 'auto-complete
  (define-key ac-completing-map "\t" 'ac-expand)
  (define-key ac-completing-map [tab] 'ac-expand)
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous))

;; better M-x, or so they say.
(after 'smex
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "C-x C-m") 'smex)
  (global-set-key (kbd "C-c C-m") 'smex))

;; This makes those windows with lists of possible commands more useful
(require-package 'guide-key)
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)

;; This makes C-c escape
;; (defun my-esc (prompt)
;;   "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
;;   (cond
;;    ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
;;    ;; Key Lookup will use it.
;;    ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
;;    ;; This is the best way I could infer for now to have C-c work during evil-read-key.
;;    ;; Note: As long as I return [escape] in normal-state, I don't need this.
;;    ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
;;    (t (kbd "C-g"))))
;; (define-key key-translation-map (kbd "C-c") 'my-esc)
;; ;; Works around the fact that Evil uses read-event directly when in operator state, which
;; ;; doesn't use the key-translation-map.
;; (define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
;; ;; Not sure what behavior this changes, but might as well set it, seeing the Elisp manual's
;; ;; documentation of it.
;; (set-quit-char "C-c")


(define-key evil-normal-state-map [escape] 'custom/evil-force-normal-state)

(define-key evil-normal-state-map "  w" 'evil-ace-jump-word-mode)
(define-key evil-normal-state-map "  b" 'evil-ace-jump-word-mode)
(define-key evil-normal-state-map "  f" 'evil-ace-jump-char-mode)
(define-key evil-normal-state-map "  F" 'evil-ace-jump-char-mode)
(define-key evil-normal-state-map "  t" 'evil-ace-jump-char-to-mode)
(define-key evil-normal-state-map "  T" 'evil-ace-jump-char-to-mode)
(define-key evil-normal-state-map "  j" 'evil-ace-jump-line-mode)
(define-key evil-normal-state-map "  k" 'evil-ace-jump-line-mode)
(define-key evil-normal-state-map "  '" 'ace-jump-mode-pop-mark)
(define-key evil-normal-state-map "  `" 'ace-jump-mode-pop-mark)

(provide 'custom-maps)
