(defmacro allow-line-as-region-for-function (orig-function)
`(defun ,(intern (concat (symbol-name orig-function) "-or-line"))
   ()
   ,(format "Like `%s', but acts on the current line if mark is not active."
            orig-function)
   (interactive)
   (if mark-active
       (call-interactively (function ,orig-function))
     (save-excursion
       (beginning-of-line)
       (set-mark (point))
       (end-of-line)
       (call-interactively (function ,orig-function))))))
(provide 'custom-utils)
