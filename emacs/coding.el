;; General

;; Check spelling in comments / docstrings
(req-package flyspell
  :diminish flyspell-mode
  :config (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )

(req-package company
  :diminish company-mode
  :require evil-leader
  :config (progn
            (global-company-mode)
            (setq company-backends '(company-semantic (company-dabbrev-code
                                                       company-gtags
                                                       company-etags
                                                       company-keywords)
                                                      company-ispell
                                                      company-files
                                                      company-dabbrev)
                  company-idle-delay nil
                  company-show-numbers t
                  )
            (evil-define-key 'insert global-map
              (kbd "C-x C-o") 'company-complete
              (kbd "C-x C-u") 'company-complete
              )
            )
  )

(req-package company-quickhelp
  :require company
  :config (company-quickhelp-mode t))

(req-package company-go
  :require (go-mode company)
  :config (add-hook 'go-mode-hook
                    (lambda ()
                      (set (make-local-variable 'company-backends)
                           '(company-go company-semantic
                                        (company-dabbrev-code company-gtags
                                                              company-etags
                                                              company-keywords)
                                        company-ispell company-files
                                        company-dabbrev))))
  )

(req-package company-elisp
  :require company
  :config (add-hook 'emacs-lisp-mode-hook
                    (lambda ()
                      (set (make-local-variable 'company-backends)
                           '(company-elisp company-semantic
                                           (company-dabbrev-code
                                            company-gtags
                                            company-etags
                                            company-keywords)
                                           company-ispell
                                           company-files
                                           company-dabbrev))))
  )

(req-package company-anaconda
  :require (anaconda-mode company)
  :config (add-hook 'python-mode-hook
                    (lambda ()
                      (set (make-local-variable 'company-backends)
                           '(company-anaconda company-semantic
                                              (company-dabbrev-code
                                               company-gtags
                                               company-etags
                                               company-keywords)
                                              company-ispell
                                              company-files
                                              company-dabbrev))))
  )

(req-package flycheck
  :diminish flycheck-mode
  :config (progn
            (add-hook 'prog-mode-hook 'flycheck-mode)
            (setq flycheck-checkers
                  (delq 'emacs-lisp-checkdoc flycheck-checkers))
            (setq flycheck-checkers
                  (delq 'puppet-parser flycheck-checkers))
            (setq flycheck-highlighting-mode nil)

            (define-fringe-bitmap 'my-flycheck-fringe-indicator
              (vector #b00000000
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00011100
                      #b00111110
                      #b00111110
                      #b00111110
                      #b00011100
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00000000))

            (flycheck-define-error-level 'error
              :overlay-category 'flycheck-error-overlay
              :fringe-bitmap 'my-flycheck-fringe-indicator
              :fringe-face 'flycheck-fringe-error)

            (flycheck-define-error-level 'warning
              :overlay-category 'flycheck-warning-overlay
              :fringe-bitmap 'my-flycheck-fringe-indicator
              :fringe-face 'flycheck-fringe-warning)

            (flycheck-define-error-level 'info
              :overlay-category 'flycheck-info-overlay
              :fringe-bitmap 'my-flycheck-fringe-indicator
              :fringe-face 'flycheck-fringe-info)
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
            (add-hook 'text-mode-hook 'whitespace-mode)

            ;; Cleanup whitespace on save
            (defun komitee/whitespace-hook ()
              (add-hook 'before-save-hook 'whitespace-cleanup nil t)
              )
            (add-hook 'prog-mode-hook 'komitee/whitespace-hook)
            (add-hook 'text-mode-hook 'komitee/whitespace-hook)

            ;; Make it disable-able
            ;; XXX This disables more than just the whitespace hook.
            (defun komitee/remove-whitespace-hook ()
              "remove whitespace cleanup hook"
              (interactive)
              (setq-local before-save-hook ())
              )
            )
  )

(defun komitee/comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1)
  (diminish 'auto-fill-function)
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

(req-package pyvenv)

(req-package eldoc
  :diminish eldoc-mode
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'python-mode-hook 'eldoc-mode)
    )
  )

(defun komitee/tabs ()
  (interactive)
  (setq-local indent-tabs-mode t)
  )

;; go
(req-package go-mode
  :require (evil exec-path-from-shell)
  :config (progn
            (evil-define-key 'motion go-mode-map
              "K" 'godoc-at-point
              "gd" 'godef-jump
              )
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
(req-package python
  :require (anaconda-mode evil)
  :config (progn
            (setq python-fill-docstring-style (quote django)
                  python-shell-interpreter "ipython"
                  )
            (add-hook 'python-mode-hook (lambda () (anaconda-mode)))
            (evil-define-key 'motion python-mode-map
              "K" 'anaconda-mode-view-doc
              "gd" 'anaconda-mode-goto-definitions
              )
            )
  )

(req-package anaconda-mode
  :diminish anaconda-mode
  )

(req-package lisp-mode
  :require evil
  :config (progn
            (evil-define-key 'normal lisp-mode-shared-map
              "K" 'elisp-slime-nav-describe-elisp-thing-at-point
              (kbd "SPC !") 'eval-last-sexp
              )
            (evil-define-key 'visual lisp-mode-shared-map
              (kbd "SPC !") 'eval-region
              )
            )
  )

(req-package puppet-mode
  :config (progn
            (setq puppet-validate-command "puppet")

            ;; This is lifted from flycheck.el so that I can disabled
            ;; autoloader_layout checks If that code changes, I'll
            ;; need to update it here.
            (flycheck-define-checker puppet-lint
              "A Puppet DSL style checker using puppet-lint."
              :command ("puppet-lint"
                        "--log-format" "%{path}:%{linenumber}:%{kind}: %{message} (%{check})"
                        "--no-autoloader_layout-check"
                        source-original)
              :error-patterns
              ((warning line-start (file-name) ":" line ":warning: " (message) line-end)
               (error line-start (file-name) ":" line ":error: " (message) line-end))
              :modes puppet-mode
              :predicate flycheck-buffer-saved-p)
            )
  )

(req-package haskell-mode
  :require evil
  :config (progn
            (evil-define-key 'normal haskell-mode-map
              (kbd "C-c C-c") 'inferior-haskell-send-decl
              )
            (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
            )
  )

(req-package sh-script
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
            (add-hook 'sh-mode-hook
                      (lambda ()
                        (if (string-match "\\.zsh$" buffer-file-name)
                            (sh-set-shell "zsh"))))
            )
  )

(req-package gdb-mi
  :config (setq gdb-many-windows t)
  )

(req-package cc-vars
  :config (progn
            (setq c-basic-offset 4)
            )

  )
