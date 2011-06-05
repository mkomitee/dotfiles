;;; Commentary:

;; mv-shell integrates with shell-mode in order to keep buffers in sync when
;; moving files around.  If you enter a 'mv' command on a file that has a buffer opened,
;; the buffer is also renamed and moved to the location the file is moved to.  Buffers are
;; also moved when a directory they are in is moved.

;;; Installation:

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;;
;; (require 'mv-shell)
;; (mv-shell-mode 1)
;;

