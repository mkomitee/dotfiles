;; General

;; Check spelling in comments / docstrings
(req-package flyspell
  :diminish flyspell-mode
  :config (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )

(req-package elec-pair
  :config (add-hook 'prog-mode-hook 'electric-pair-mode)
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

;; make
(req-package make-mode
  :config (add-hook 'makefile-mode-hook 'komitee/tabs)
  )

;; python
(req-package python
  :config (progn
            (setq python-fill-docstring-style (quote django)
                  python-shell-interpreter "ipython"
                  )
            (evil-define-key 'normal python-mode-map
              (kbd "K") 'python-eldoc-function)
            )
            (add-hook 'python-mode-hook (lambda () (run-python "ipython" t nil)))
  )

(req-package lisp-mode
  :config (evil-define-key 'normal emacs-lisp-mode-map
            (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point)
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
