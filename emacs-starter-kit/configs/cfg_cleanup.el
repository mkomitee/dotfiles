; Common LISP extensions package
(require 'cl)

;; Don't add newlines if you do next-line at the end of a buffer.
(setq next-line-add-newlines nil)

;; Preserve file owners and permissions when saving a backup.
(setq backup-by-copying-when-mismatch t)

;; Turn off auto-save.
(setq auto-save-default nil)

;; Where do we allow backup, auto-save and lock files?  By default, only
;; in one's home directory (excluding root).
(defvar cruft-enabled-regexp
  (if (equal (user-login-name) "root")
      nil
    (expand-file-name "~"))
  "Enable backup (~) files for filenames matching this regexp.")

;; Turn off backups where not desired.
(setq backup-enable-predicate
      (function
       (lambda (name)
         (if name
             (let ((backup-ok
                    (and
                     cruft-enabled-regexp
                     (string-match cruft-enabled-regexp
                                   (expand-file-name name)))))
               (unless backup-ok
                 (message
                  "Emacs will not make backups when saving this file."))
               ;; return whether to do backup or not
               backup-ok)))))

;; Set auto-save and locks only where desired.
;;
;; File locking is a compile-time option; it can't be shut off.  So, we
;; contrive to delete the locks whenever they occur.
;;
;; There is no hook called after a buffer becomes modified, just before.
;; And there's a hook called after *every* modification, but we don't want
;; to do anything complicated there.  So on first-modify we set a flag
;; telling our every-modify hook when to unlock the buffer.

(defvar inhibit-locks-on-next-modify nil
  "set when a buffer has changed, and should be unlocked")

;; hooks run when an unmodified buffer is about to modified
(add-hook
 'first-change-hook
 (function
  (lambda ()
    (let ((name (buffer-file-name (current-buffer))))
      (if name
          (let ((file (expand-file-name name)))
            ;; unless this location is cruft-enabled, flag to unlock
            (unless
                (and
                 cruft-enabled-regexp
                 (string-match cruft-enabled-regexp file))
              (setq inhibit-locks-on-next-modify t))))))))

;; hooks run after every buffer change
(add-hook
 'after-change-functions
 (function
  (lambda (begin end length)
    ;; if flagged
    (if inhibit-locks-on-next-modify
        (progn
          ;; unflag
          (setq inhibit-locks-on-next-modify nil)
          ;; and delete the lock
          (unlock-buffer))))))

;; hooks run visiting a file
(add-hook
 'find-file-hooks
 (function
  (lambda ()
    (let ((name (buffer-file-name (current-buffer))))
      (if name
          (let ((file (expand-file-name name)))
            ;; turn on auto-save iff this location is cruft-enabled,
            (auto-save-mode
             (if (string-match cruft-enabled-regexp file)
                 1 0))))))))

;; Don't trust the RCS headers to determine version control state.
(setq vc-consult-headers nil)
;; Don't trust owner/permissions to determine version control state.
(setq vc-mistrust-permissions t)
;; Overwrite working files on checking out without complaining.
(setq vc-checkout-switches '("-f"))

;;; crypt++ / gpg

(if (load "crypt++" t)
    (progn
      (setq crypt-encryption-type 'gpg)
      (setq crypt-confirm-password t)
      (crypt-rebuild-tables)))

;;; autoloading functions

(autoload 'longlines-mode "longlines"
  "soft word wrap (newlines only at end of paragraph)" t)

;;; routines

(defun buffer-translate-ip-addresses ( &optional display )
  "Look for IP addresses in current buffer, and replace them with reverse DNS lookups. \
Prefix number of seconds to wait & redisplay."
  (interactive "*P")
  (flet ((ip-address-p ;;does this list of numbers look like the octets of an IP address
          (octet-list)
          ;; if first octet is 255, probably (surely?) a mask, not an address
          (if (= (car octet-list) 255)
              nil
            ;; check that the rest are in [0,255]
            (let ((yes t))
              (progn
                (mapc (lambda (n)
                        (unless (and (>= n 0)
                                     (<= n 255))
                          (setq yes nil)))
                      (cdr octet-list))
                yes))))
         ;; look up hostname using "dig" (with caching)
         (get-hostname
          (address-string)
          (let* ((hash-lookup (gethash address-string address-hash))
                 ;; do the reverse lookup with "dig"
                 (dig-output
                  (shell-command-to-string
                   (concat "dig +short +time=1 -x " address-string)))
                 (hostname-maybe
                  (if (<= (length dig-output) 1) nil
                    ;; (remove trailing dot & newline)
                    (substring dig-output 0 -2))))
            (puthash address-string hostname-maybe address-hash))))
    ;; (end flet) -- create caching hash table
    (let ((address-hash (make-hash-table :test 'equal)))
      ;; loop looking for addresses
      (while (re-search-forward
              "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" nil t)
        (let ((address-string (match-string 0))	; get the whole address string
              ;; get the octets as integers
              (octets (mapcar (lambda (n) (string-to-int (match-string n)))
                              '(1 2 3 4))))
          ;; if we've really got an address
          (if (ip-address-p octets)
              ;; look for a hostname
              (let ((hostname (get-hostname address-string)))
                (if hostname		;if we got one
                    (progn
                      (replace-match hostname) ;replace with it
                      (if display
                          (sit-for (prefix-numeric-value display))))))))))))
