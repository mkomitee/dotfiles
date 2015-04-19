(req-package linum
  :config (global-linum-mode t)
  )

;; If we have a fringe, make it 8 pixels wide
(if (featurep 'fringe)
    (fringe-mode 16))

(req-package color-theme-sanityinc-solarized
  :config (load-theme 'sanityinc-solarized-light t)
  )

;; base16-theme doesn't provide 'base16-theme. Instead it includes
;; files which provide-theme 'base16-*, so base16-theme is installed
;; in packages.el and we load it here.

;; moe-theme / moe-light

;; leuven-theme / leuven

;; darktooth-theme / darktooth

;; gruvbox / gruvbox

;; atom-dark-theme / atom-dark

;; molokai-theme

;; moe-theme / moe-dark

;; base16-theme / base16-ocean

;; base16-theme / base16-default
