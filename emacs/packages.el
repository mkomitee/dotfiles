(setq package-archives
      (quote
       (("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
      )
(require 'package)
(package-initialize)

(dolist (package '(req-package
                    ;; base16-theme doesn't actually provide
                    ;; base16-theme, so it doesn't work w/ req-package, Yay.
                    base16-theme))
  (unless (package-installed-p package)
    (package-install package)))

(require 'req-package)
