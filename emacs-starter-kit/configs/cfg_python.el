(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'flymake-mode)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)

;(add-to-list 'load-path "~/.emacs.d/vendor/Pymacs")
;(add-to-list 'load-path "~/.emacs.d/vendor/python-mode")
(setq pymacs-load-path '("~/.emacs.d/vendor/python-mode"))

;; python-mode settings
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist(cons '("python" . python-mode)
                                  interpreter-mode-alist))
;; path to the python interpreter, e.g.: ~rw/python27/bin/python2.7
(setq py-python-command "python")
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; pymacs settings
(setq pymacs-python-command py-python-command)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")

(require 'pycomplete)
