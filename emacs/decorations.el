(req-package linum)

;; If we have a fringe, make it 8 pixels wide
(if (featurep 'fringe)
    (fringe-mode '(8 . 0)))

;; base16-theme doesn't provide 'base16-theme. Instead it includes
;; files which provide-theme 'base16-*, so base16-theme is installed
;; in packages.el and we load it here.
(load-theme 'base16-default t)
;; molokai-theme
;; moe-theme / moe-dark
;; base16-theme / base16-ocean
;; base16-theme / base16-default
