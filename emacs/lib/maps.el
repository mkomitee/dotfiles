;; Like C-p in vim
(define-key global-map (kbd "C-c b") 'switch-to-buffer)

(define-key global-map (kbd "C-;") 'comment-or-uncomment-region-or-line)

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

;; All characters will automatically be prefixed w/ C- unless they start with
;; g, which will automatically prefix the following character w/ M-
(define-key global-map (kbd "C-c g") 'god-local-mode)

;; Recommended by the author of god-mode
(define-key god-local-mode-map (kbd ".") 'repeat)

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
(define-key global-map (kbd "S-C-<down>") 'shrink-window-vertically)
(define-key global-map (kbd "S-C-<up>") 'enlarge-window-vertically)

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
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)

;; Using flycheck instead of flymake
(define-key elpy-mode-map (kbd "C-c C-n") 'flycheck-next-error)
(define-key elpy-mode-map (kbd "C-c C-p") 'flycheck-previous-error)

;; Expand Region -- note, C-- C-= will contract the region.
(define-key global-map (kbd "C-=") 'er/expand-region)
