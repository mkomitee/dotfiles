(req-package helm
  :require evil-leader
  :config (progn
            (setq helm-split-window-in-side-p t
                  helm-move-to-line-cycle-in-source t
                  helm-ff-search-library-in-sexp t
                  helm-ff-file-name-history-use-recentf t
                  helm-autoresize-max-height 20
                  helm-M-x-fuzzy-match t
                  helm-ag-use-grep-ignore-list t
                  helm-ff-skip-boring-files t
                  )
            (evil-leader/set-key
              "ff" 'helm-find-files
              "fb" 'helm-buffers-list
              "fr" 'helm-recentf
              "fk" 'helm-show-kill-ring
              "ev" (lambda () (interactive)
                     (helm-find-files-1 (expand-file-name
                                         komitee/emacs-config-directory)))
              )
            (helm-autoresize-mode t)
            (global-set-key (kbd "M-x") 'helm-M-x)
            )
  )

(req-package helm-ag
  :require helm
  )

(req-package helm-company
  :require helm company
  :config (progn
            (evil-define-key 'insert global-map
              (kbd "C-x C-o") 'helm-company
              (kbd "C-x C-u") 'helm-company
              )
            )
  )

(req-package helm-flyspell
  :require helm flyspell
  :config (progn
            )
  )

(req-package helm-flycheck
  :require helm flycheck
  :config (progn
            )
  )

(req-package helm-projectile
  :require helm projectile helm-ag
  :config (progn
            (evil-leader/set-key
              "fp" 'helm-projectile-find-file
              "fP" 'helm-projectile-switch-project
              "fB" 'helm-projectile-switch-to-buffer
              "fR" 'helm-projectile-recentf
              "//" 'helm-projectile-ag
              )
            )
  )
