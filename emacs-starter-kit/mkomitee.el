;;; Enable viper mode
;(setq viper-custom-file-name (concat dotfiles-dir "viper"))
;(setq viper-mode t)
;(require 'viper)

;;; And vimpulse mode for good measure
;(require 'vimpulse)

;;; Themes, ...
(require 'color-theme)
;(require 'color-theme-ir-black)
;(color-theme-ir-black)
(require 'color-theme-solarized)
(color-theme-solarized-light)

;;; Line numbers, ...
(setq linum-format "%5.d ")
(global-linum-mode 1)

;;; Setup spaces/tabs
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq c-basic-indent 4)

(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'flymake-mode)
(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'flymake-mode)
(add-hook 'perl-mode-hook 'whitespace-mode)
(add-hook 'perl-mode-hook 'flymake-mode)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/")

(server-start)
