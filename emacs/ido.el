;; interatively do things ...
(req-package ido
  :require evil-leader
  :config (progn
            (setq ido-save-directory-list-file "~/.emacs.d/.ido.last"
                  ido-ignore-buffers (quote ("\\` "
                                             "\\*Python\\[.*\\]\\*"))
                  )
            ;; (evil-leader/set-key
            ;;   "ff" 'ido-find-file
            ;;   "fb" 'ido-switch-buffer
            ;;   "ev" (lambda () (interactive)
            ;;          (ido-find-file-in-dir (expand-file-name
            ;;                                 komitee/emacs-config-directory)))
            ;;   )
            (ido-everywhere t)
            )
  )

(req-package flx-ido
  :require ido
  :config (progn
            (flx-ido-mode t)
            (setq ido-enable-flex-matching t)
            (setq ido-use-faces nil)
            )
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
