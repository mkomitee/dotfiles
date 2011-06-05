;;; perlcritic-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (perlcritic-mode perlcritic-region perlcritic)
;;;;;;  "perlcritic" "perlcritic.el" (19947 6249))
;;; Generated autoloads from perlcritic.el

(autoload 'perlcritic "perlcritic" "\
\\[perlcritic]] returns a either nil or t depending on whether the
current buffer passes perlcritic's check. If there are any warnings
those are displayed in a separate buffer.

\(fn)" t nil)

(autoload 'perlcritic-region "perlcritic" "\
\\[perlcritic-region] returns a either nil or t depending on
whether the region passes perlcritic's check. If there are any
warnings those are displayed in a separate buffer.

\(fn START END)" t nil)

(autoload 'perlcritic-mode "perlcritic" "\
Perl::Critic checking minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("perlcritic-pkg.el") (19947 6249 716466))

;;;***

(provide 'perlcritic-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; perlcritic-autoloads.el ends here
