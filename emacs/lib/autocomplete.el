(defvar ac-auto-show-menu t)
(defvar ac-auto-start 1)
(defvar ac-comphist-file (concat user-emacs-directory ".cache/ac-comphist.dat"))
(defvar ac-quick-help-prefer-pos-tip nil)
(defvar ac-max-width 0.5)
(defvar ac-show-menu-immediately-on-auto-complete t)

(require 'auto-complete-config)
(ac-config-default)

(after 'linum (ac-linum-workaround))

(defadvice ac-expand (before advice-for-ac-expand activate)
  (when (yas-expand)
    (ac-stop)))