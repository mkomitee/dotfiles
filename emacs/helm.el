(req-package helm
  :require evil-leader
  :diminish helm-mode
  :bind (
         ("M-x" . helm-M-x)
         )
  :config (progn
            (global-set-key (kbd "C-c h") 'helm-command-prefix)
            (global-unset-key (kbd "C-x c"))
            (evil-leader/set-key
              "b" 'helm-buffers-list
              )
            (when (executable-find "curl")
              (setq helm-google-suggest-use-curl-p t)
              )
            (setq helm-move-to-line-cycle-in-source t
                  helm-ff-file-name-history-use-recentf t
                  helm-ff-skip-boring-files t
                  helm-buffer-max-length nil
                  helm-buffer-details-flag nil
                  helm-boring-buffer-regexp-list (append
                                                  helm-boring-buffer-regexp-list
                                                  '("\\*Python\\["))

                  )
            (evil-leader/set-key
              "f" 'helm-find-files
              "Hf" 'helm-find-files
              "ev" (lambda () (interactive)
                     (helm-find-files-1 (expand-file-name
                                         komitee/emacs-config-directory)))
              "Hk" 'helm-show-kill-ring
              )
            (helm-mode 1)
            (helm-autoresize-mode t)
            )
  )

(req-package helm-ag
  :require helm evil evil-leader
  :config (progn
            (evil-leader/set-key
              "HA" 'helm-ag
              )
            (evil-ex-define-cmd "ag" 'helm-ag)
            )
  )

(req-package helm-projectile
  :require evil-leader helm projectile helm-ag
  :config (evil-leader/set-key
            "p" 'helm-projectile
            "P" 'helm-projectile-switch-project
            "B" 'helm-projectile-switch-to-buffer
            "r" 'helm-projectile-recentf
            "/" 'helm-projectile-ag
            "Ha" 'helm-projectile-ag
            )
  )

(req-package helm-c-yasnippet
  :require evil-leader yasnippet helm
  :config (evil-leader/set-key
            "Hy" 'helm-yas-complete
            )
  )
