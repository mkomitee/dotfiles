;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1)))
  )

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring")
  )

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1)
  )

(defun komitee/compile-elisp ()
  "Byte compile dotfile directories"
  (interactive)
  (progn
    (byte-recompile-directory komitee/emacs-config-directory 0)
    (byte-compile-file komitee/local-emacs-config)
    )
  )

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit))
  )

(defun komitee/nohl-quit ()
  "nohl & then quit; when used in normal mode will cancel search highlighting."
  (interactive)
  (progn
    (evil-search-highlight-persist-remove-all)
    (evil-ex-nohighlight)
    (keyboard-quit)
    )
  )

(defun komitee/split-horizontally ()
    (interactive)
  (progn
    (split-window-horizontally)
    (other-window 1)
    )
  )

(defun komitee/split-vertically ()
    (interactive)
  (progn
    (split-window-vertically)
    (other-window 1)
    )
  )

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun komitee/split-file (file)
  "Read the contents of a file and return as a list."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string)))))
