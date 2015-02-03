;; interatively do things ...
(req-package ido
  :config (progn
            (setq ido-save-directory-list-file "~/.emacs.d/.ido.last"
                  ido-use-filename-at-point (quote guess)
                  ido-ignore-buffers (quote ("\\` "
                                             "\\*Python\\[.*\\]\\*"))
                  )
            (ido-everywhere t)
            )
  )

(req-package flx-ido
  :require ido
  :config (flx-ido-mode t)
  )

(req-package ido-hacks
  :require ido
  )

(req-package ido-ubiquitous
  :require ido
  :config (ido-ubiquitous-mode t)
  )

(req-package ido-vertical-mode
  :require ido
  :config (ido-vertical-mode)
  )
