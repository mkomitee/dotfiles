;;; Enable viper mode
(setq viper-custom-file-name (concat dotfiles-dir "viper"))
(setq viper-mode t)
(require 'viper)

;;; And vimpulse mode for good measure
(require 'vimpulse)

;;; Themes, ...
(require 'color-theme-ir-black)
(color-theme-ir-black)

;;; Line numbers, ...
(setq linum-format "%5.d ")
(global-linum-mode 1)

;;; Show white-space
(require 'blank-mode)

;;; Setup spaces/tabs
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq c-basic-indent 4)
