;;; Themes, ...
;(require 'color-theme)
;;(require 'color-theme-solarized)
;;(color-theme-solarized-light)
(require 'color-theme-ir-black)
(color-theme-ir-black)


;;; Line numbers, ...
(setq linum-format "%5.d ")
(global-linum-mode 1)

;;; Setup spaces/tabs
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq c-basic-indent 4)

;;; Only first learning emacs, use viper as a crutch
(setq viper-mode t)
(setq viper-custom-file-name "~/.emacs.d/viper")
(require 'viper)
(require 'vimpulse)
