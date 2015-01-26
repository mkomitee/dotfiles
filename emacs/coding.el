;; General

;; Check spelling in comments / docstrings
(req-package flyspell
  :diminish flyspell-mode
  :config (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )

(req-package elec-pair
  :config (progn
            (defun komitee/disable-electric-pair-mode ()
              (interactive)
              (set (make-local-variable 'electric-pair-mode) nil)
              )
            (add-hook 'prog-mode-hook 'electric-pair-mode)
            (add-hook 'minibuffer-setup-hook 'komitee/disable-electric-pair-mode)
            )
  )

(req-package company
  :require evil
  :diminish company-mode
  :config (progn
            (global-company-mode)
            (setq company-backends '(company-semantic (company-dabbrev-code
                                                       company-gtags
                                                       company-etags
                                                       company-keywords)
                                                      company-files
                                                      company-dabbrev)
                  company-idle-delay nil
                  )
            (define-key evil-insert-state-map (kbd "C-x C-o") 'company-complete)
            (define-key evil-insert-state-map (kbd "C-x C-u") 'company-complete)
            )
  )

(req-package company-go
  :require (go-mode company)
  :config (add-hook 'go-mode-hook
                    (lambda ()
                      (set (make-local-variable 'company-backends)
                           '(company-go company-semantic
                                        (company-dabbrev-code company-gtags
                                                              company-etags
                                                              company-keywords)
                                        company-files company-dabbrev))))
  )

(req-package company-elisp
  :require company
  :config (add-hook 'emacs-lisp-mode-hook
                    (lambda ()
                      (set (make-local-variable 'company-backends)
                           '(company-elisp company-semantic
                                           (company-dabbrev-code company-gtags
                                                                 company-etags
                                                                 company-keywords)
                                           company-files company-dabbrev))))
  )

(req-package company-anaconda
  :require company
  :config (add-hook 'python-mode-hook
                    (lambda ()
                      (set (make-local-variable 'company-backends)
                           '(company-anaconda company-semantic
                                              (company-dabbrev-code company-gtags
                                                                    company-etags
                                                                    company-keywords)
                                              company-files company-dabbrev))))
  )

(req-package flycheck
  :diminish flycheck-mode
  :config (progn
            (add-hook 'prog-mode-hook 'flycheck-mode)
            (setq flycheck-checkers
                  (delq 'emacs-lisp-checkdoc flycheck-checkers)
                  )
            )
  )

;; No tabs while coding
(defun komitee/notabs ()
  (interactive)
  (setq-local indent-tabs-mode nil)
  )
(add-hook 'prog-mode-hook 'komitee/notabs)

;; Manage whitespace
(req-package whitespace
  :diminish whitespace-mode
  :config (progn
            (setq whitespace-style
                  (quote
                   (face faces space-before-tab tab-mark tabs trailing))
                  )
            (add-hook 'prog-mode-hook 'whitespace-mode)

            ;; Cleanup whitespace on save
            (defun komitee/whitespace-hook ()
              (add-hook 'before-save-hook 'whitespace-cleanup nil t)
              )
            (add-hook 'prog-mode-hook 'komitee/whitespace-hook)

            ;; Make it disable-able
            ;; XXX This disables more than just the whitespace hook.
            (defun komitee/remove-whitespace-hook ()
              "remove whitespace cleanup hook"
              (interactive)
              (setq-local before-save-hook ())
              )
            )
  )

;; Display a thin red vertical line at the 80th column
(req-package fill-column-indicator
  :diminish fci-mode
  :config (progn
            (setq fci-rule-character 9550
                  fci-rule-color "darkred"
                  fci-rule-column 80
                  )
            (defun komitee/fci-hook ()
              (progn
                (turn-on-fci-mode)
                )
              )
            (add-hook 'prog-mode-hook 'komitee/fci-hook)
            )
  )


(defun komitee/comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1)
  )
(add-hook 'prog-mode-hook 'komitee/comment-auto-fill)

(req-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(req-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :config (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  )

(req-package eldoc
  :diminish eldoc-mode
  :config (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  )

(defun komitee/tabs ()
  (interactive)
  (setq-local indent-tabs-mode t)
  )

;; go
(req-package go-mode
  :require exec-path-from-shell
  :config (progn
            (evil-define-key 'normal go-mode-map "K" 'godoc-at-point)
            (exec-path-from-shell-copy-env "GOROOT")
            (exec-path-from-shell-copy-env "GOPATH")
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook 'gofmt-before-save)
            (add-hook 'go-mode-hook 'komitee/tabs)
            )
  )

;; make
(req-package make-mode
  :config (add-hook 'makefile-mode-hook 'komitee/tabs)
  )

;; python
(defun komitee/python-eldoc-at-point ()
  (interactive)
  (python-eldoc-at-point (python-info-current-symbol))
  )

(req-package python
  :config (progn
            (setq python-fill-docstring-style (quote django)
                  python-shell-interpreter "ipython"
                  )
            (add-hook 'python-mode-hook (lambda () (run-python "ipython" t nil)))
            (evil-define-key 'normal python-mode-map
              "K" 'komitee/python-eldoc-at-point
              )
            )
  )

(req-package lisp-mode
  :config (progn
            (evil-define-key 'normal lisp-mode-shared-map
              "K" 'elisp-slime-nav-describe-elisp-thing-at-point
              (kbd "SPC !") 'eval-last-sexp
              )
            (evil-define-key 'visual lisp-mode-shared-map
              (kbd "SPC !") 'eval-region)
            )
  )

(req-package puppet-mode)

(req-package haskell-mode
  :require evil
  :config (progn
            (evil-define-key 'normal
              haskell-mode-map (kbd "C-c C-c") 'inferior-haskell-send-decl)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
            )
  )
