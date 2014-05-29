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

;; Insidious
(define-key global-map (kbd "C-c v %") 'evilmi-jump-items)
(define-key global-map (kbd "C-c v J") 'evil-join)
(define-key global-map (kbd "C-c v gu") 'evil-downcase)
(define-key global-map (kbd "C-c v gU") 'evil-upcase)
(define-key global-map (kbd "C-c v ~") 'evil-invert-char)
(define-key global-map (kbd "C-c v g~") 'evil-invert-case)
(define-key global-map (kbd "C-c v <") 'evil-shift-left)
(define-key global-map (kbd "C-c v >") 'evil-shift-right)
(define-key global-map (kbd "C-c v g;") 'goto-last-change)
(define-key global-map (kbd "C-c v g,") 'goto-last-change-reverse)
(define-key global-map (kbd "C-c v u") 'undo)
(define-key global-map (kbd "C-c v C-r") 'redo)
(define-key global-map (kbd "C-c v C-w h") 'evil-window-left)
(define-key global-map (kbd "C-c v C-h") 'evil-window-left)
(define-key global-map (kbd "C-c v C-w j") 'evil-window-down)
(define-key global-map (kbd "C-c v C-j") 'evil-window-down)
(define-key global-map (kbd "C-c v C-w k") 'evil-window-up)
(define-key global-map (kbd "C-c v C-k") 'evil-window-up)
(define-key global-map (kbd "C-c v C-w l") 'evil-window-right)
(define-key global-map (kbd "C-c v C-l") 'evil-window-right)
(define-key global-map (kbd "C-c v f") 'evil-find-char)
(define-key global-map (kbd "C-c v F") 'evil-find-char-backward)
(define-key global-map (kbd "C-c v t") 'evil-find-char-to)
(define-key global-map (kbd "C-c v T") 'evil-find-char-to-backward)
(define-key global-map (kbd "C-c v ;") 'evil-repeat-find-char)
(define-key global-map (kbd "C-c v ,") 'evil-repeat-find-char-reverse)
(define-key global-map (kbd "C-c v *") 'evil-search-word-forward)
(define-key global-map (kbd "C-c v #") 'evil-search-word-backward)
