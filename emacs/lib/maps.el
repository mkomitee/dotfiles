;; Like C-p in vim
(define-key global-map (kbd "C-c b") 'switch-to-buffer)
(define-key global-map (kbd "C-c f") 'projectile-find-file)
(define-key global-map (kbd "C-c r") 'projectile-recentf)

(define-key global-map (kbd "C-;") 'comment-or-uncomment-region-or-line)

;; Allows completion for commands.
(define-key global-map (kbd "M-x") 'smex)
(define-key global-map (kbd "C-x C-m") 'smex)
(define-key global-map (kbd "C-c C-m") 'smex)
(define-key global-map (kbd "C-c /") 'ag-regexp-project-at-point)

;; This is like w and f in easy-motion in vim.
(define-key global-map (kbd "C-c w") 'ace-jump-word-mode)
(define-key global-map (kbd "C-c c") 'ace-jump-char-mode)

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

;; http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
(define-key global-map (kbd "C-x C-k") 'delete-window)
(define-key global-map (kbd "M-o") 'other-window)
(define-key global-map (kbd "S-C-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "S-C-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "S-C-<down>") 'shrink-window-vertically)
(define-key global-map (kbd "S-C-<up>") 'enlarge-window-vertically)

;; http://www.masteringemacs.org/articles/2010/10/04/beginners-guide-to-emacs/
(define-key global-map (kbd "RET") 'newline-and-indent)

;; This makes those windows with lists of possible commands more useful
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)
