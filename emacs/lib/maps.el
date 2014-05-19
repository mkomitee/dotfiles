;; Like C-p in vim
(global-set-key (kbd "C-c b") 'switch-to-buffer)
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c r") 'projectile-recentf)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; Allows completion for commands.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "C-c /") 'ag-regexp-project-at-point)

;; This is like w and f in easy-motion in vim.
(global-set-key (kbd "C-c w") 'ace-jump-word-mode)
(global-set-key (kbd "C-c c") 'ace-jump-char-mode)

;; All characters will automatically be prefixed w/ C- unless they start with
;; g, which will automatically prefix the following character w/ M-
(global-set-key (kbd "C-c g") 'god-local-mode)

;; Recommended by the author of god-mode
(define-key god-local-mode-map (kbd ".") 'repeat)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(global-set-key (kbd "C-c |") 'split-window-horizontally)
(global-set-key (kbd "C-c _") 'split-window-vertically)

(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c C-k") 'delete-window)

;; This makes those windows with lists of possible commands more useful
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "g"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)
