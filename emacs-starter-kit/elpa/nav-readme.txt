;;; Commentary:
;; 
;; To use this file, put something like the following in your
;; ~/.emacs:
;;
;; (add-to-list 'load-path "/directory/containing/nav/")
;; (require 'nav)
;;
;; Type M-x nav to open the navigation window. It should show up as a
;; 30-character wide column on the left, showing the contents of the
;; current directory. If there are multiple windows open, all but one
;; will be closed to make sure the nav window shows up correctly.

;;; Key Bindings
;;
;;   Enter/Return: Open file or directory under cursor.
;;
;;   1: Open file under cursor in 1st other window.
;;   2: Open file under cursor in 2nd other window.
;;
;;   c: Copy file or directory under cursor.
;;   d: Delete file or directory under cursor (asks to confirm first).
;;   e: Edit current directory in dired.
;;   f: Recursively find files whose names or contents match some regexp.
;;   g: Recursively grep for some regexp.
;;   j: Jump to another directory.
;;   m: Move or rename file or directory.
;;   n: Make new directory.
;;   p: Pop directory stack to go back to the directory where you just were.
;;   q: Quit nav.
;;   r: Refresh.
;;   s: Start a shell in an emacs window in the current directory.
;;   t: Start a terminal in an emacs window in the current directory.
;;      This allows programs like vi and less to run. Exit with C-d C-d.
;;   u: Go up to parent directory.
;;   !: Run shell command.
;;   [: Rotate non-nav windows counter clockwise.
;;   ]: Rotate non-nav windows clockwise.
;;

