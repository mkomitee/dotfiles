;;; Make custom libraries available
(add-to-list 'load-path (concat dotfiles-dir "/custom"))

;;; Enable viper mode
(setq viper-custom-file-name (concat dotfiles-dir "viper"))
(setq viper-mode t)
(require 'viper)

;;; And vimpulse mode for good measure
(require 'vimpulse)

;;; Themes, ...
(require 'color-theme-ir-black)
(color-theme-ir-black)

(setq tab-width 4)
(setq c-basic-offset 4)
(setq indent-tabs-mode t)
(setq linum-format "%5.d ")
(global-linum-mode 1)
