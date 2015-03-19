(req-package helm
  :require evil-leader
  :config (progn
            (setq helm-split-window-in-side-p t
                  helm-move-to-line-cycle-in-source t
                  helm-ff-search-library-in-sexp t
                  helm-ff-file-name-history-use-recentf t
                  helm-autoresize-max-height 40
                  helm-autoresize-min-height 10
                  helm-M-x-fuzzy-match t
                  helm-ag-use-grep-ignore-list t
                  helm-ff-skip-boring-files t
                  helm-display-header-line nil
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

            (defun helm-toggle-header-line ()
              (if (= (length helm-sources) 1)
                  (set-face-attribute 'helm-source-header nil :height 0.1)
                (set-face-attribute 'helm-source-header nil :height 1.0)))

            (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)
            )
  )

(req-package helm-ag
  :require helm
  )

(req-package helm-company
  :require (helm company)
  :config (progn
            (evil-define-key 'insert global-map
              (kbd "C-x C-o") 'helm-company
              (kbd "C-x C-u") 'helm-company
              )
            )
  )

(req-package helm-flyspell
  :require (helm flyspell)
  :config (progn
            (evil-define-key 'normal global-map
              "z=" 'helm-flyspell-correct
              )
            )
  )

(req-package helm-flycheck
  :require (helm flycheck)
  :config (progn
            (evil-leader/set-key
              "?" 'helm-flycheck)
            )
  )

(req-package helm-projectile
  :require (helm projectile helm-ag)
  :config (progn
            (evil-leader/set-key
              "fp" 'helm-projectile-find-file
              "fP" 'helm-projectile-switch-project
              "fB" 'helm-projectile-switch-to-buffer
              "fR" 'helm-projectile-recentf
              "//" 'helm-projectile-ag
              "/py" (lambda () (interactive)
                      (let ((helm-ag-command-option "--python"))
                        (helm-projectile-ag)
                        )
                      )
              "/el" (lambda () (interactive)
                      (let ((helm-ag-command-option "--elisp"))
                        (helm-projectile-ag)
                        )
                      )
              "/sh" (lambda () (interactive)
                      (let ((helm-ag-command-option "--shell"))
                        (helm-projectile-ag)
                        )
                      )
              "/rb" (lambda () (interactive)
                      (let ((helm-ag-command-option "--ruby"))
                        (helm-projectile-ag)
                        )
                      )
              "/pl" (lambda () (interactive)
                      (let ((helm-ag-command-option "--perl"))
                        (helm-projectile-ag)
                        )
                      )
              "/java" (lambda () (interactive)
                        (let ((helm-ag-command-option "--java"))
                          (helm-projectile-ag)
                          )
                        )
              "/hs" (lambda () (interactive)
                      (let ((helm-ag-command-option "--haskell"))
                        (helm-projectile-ag)
                        )
                      )
              "/html" (lambda () (interactive)
                        (let ((helm-ag-command-option "--html"))
                          (helm-projectile-ag)
                          )
                        )
              "/go" (lambda () (interactive)
                      (let ((helm-ag-command-option "--go"))
                        (helm-projectile-ag)
                        )
                      )
              )
            )
  )

(req-package helm-swoop
  :config (progn
            (evil-leader/set-key
              "sw" 'helm-swoop
              )
            (setq helm-swoop-pre-input-function
                  (lambda () ""))
            )
  )
