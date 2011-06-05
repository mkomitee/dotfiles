;;; Themes, ...
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-solarized-light)

;;; Snippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/")

;;; Textmate Mode
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(textmate-mode)

;;; Line numbers, ...
(setq linum-format "%5.d ")
(global-linum-mode 1)

;;; Setup spaces/tabs
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq c-basic-indent 4)

;;; Ruby Config
(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'flymake-mode)
(add-hook 'ruby-mode-hook 'rainbow-delimiters-mode)

;;; Python Config
(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'flymake-mode)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)

;;; Perl Config
(add-hook 'perl-mode-hook 'whitespace-mode)
(add-hook 'perl-mode-hook 'flymake-mode)
(add-hook 'perl-mode-hook 'rainbow-delimiters-mode)

(server-start)
