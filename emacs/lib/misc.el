 (when (fboundp 'winner-mode)
      (winner-mode 1))

(defvar ag-highlight-search t)

(setq auto-mode-alist
      (cons '("\\.vim\\'" . vimrc-mode) auto-mode-alist))

(defvar magit-diff-options '("--histogram"))

(if (display-graphic-p)
    (progn
      (require 'git-gutter-fringe+)
      (global-git-gutter+-mode)))
