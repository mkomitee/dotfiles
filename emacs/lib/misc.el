 (when (fboundp 'winner-mode)
      (winner-mode 1))

(require-package 'ace-jump-mode)

(defvar ag-highlight-search t)
(require-package 'ag)

(require-package 'vimrc-mode)
(setq auto-mode-alist
      (cons '("\\.vim\\'" . vimrc-mode) auto-mode-alist))

(require-package 'magit)
(require-package 'gist)

(defvar magit-diff-options '("--histogram"))

(if (display-graphic-p)
    (progn
      (require-package 'git-gutter-fringe+)
      (require 'git-gutter-fringe+))
  (require-package 'git-gutter+))

(global-git-gutter+-mode)
