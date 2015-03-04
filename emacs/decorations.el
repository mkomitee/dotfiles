(req-package linum)

;; If we have a fringe, make it 8 pixels wide
(if (featurep 'fringe)
    (fringe-mode 0))

(req-package atom-dark-theme
  :config (load-theme 'atom-dark t)
  )

;; base16-theme doesn't provide 'base16-theme. Instead it includes
;; files which provide-theme 'base16-*, so base16-theme is installed
;; in packages.el and we load it here.
;; atom-dark
;; molokai-theme
;; moe-theme / moe-dark
;; base16-theme / base16-ocean
;; base16-theme / base16-default
