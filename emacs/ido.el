;; interatively do things ...
(req-package ido evil-leader
  :config (progn
            (setq ido-save-directory-list-file "~/.emacs.d/.ido.last"
                  ido-ignore-buffers (quote ("\\` "
                                             "\\*Python\\[.*\\]\\*"))
                  )
            (evil-leader/set-key
              "f" 'ido-find-file
              "b" 'ido-switch-buffer
              "ev" (lambda () (interactive)
                     (ido-find-file-in-dir (expand-file-name
                                            komitee/emacs-config-directory)))


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
