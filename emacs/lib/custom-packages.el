;; These are the packages I want installed
(defvar my-packages '(
                      ;; haskell-mode
                      ;; puppet-mode
                      textmate
                      ;; ag
                      ;; markdown-mode
                      ;; git-gutter
                      ;; yasnippet
                      ;; starter-kit
                      ;; starter-kit-bindings
                      ;; starter-kit-eshell
                      ;; starter-kit-lisp
                      )
  "A list of packages to ensure are installed at launch.")

;; This ensures they're installed
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'custom-packages)
