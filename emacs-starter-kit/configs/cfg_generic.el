;;; Themes, ...
(require 'color-theme-ir-black)
(color-theme-ir-black)

;;; Line numbers, ...
(setq linum-format "%5.d ")
(global-linum-mode 1)

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at
        the end of the line, then comment current line.

        Replaces default behaviour of comment-dwim, when it inserts comment at the
        end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "C-x C-;") 'comment-dwim-line)

;;; Setup spaces/tabs
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq c-basic-indent 4)

(setq speedbar-tag-split-minimum-length 100)
;;; Only first learning emacs, use viper as a crutch
;(setq viper-mode t)
;(setq viper-custom-file-name "~/.emacs.d/viper")
;(require 'viper)
;(require 'vimpulse)
;(vimpulse-vmap ",c" 'comment-dwim)

(setq
  user-temporary-file-directory "~/.tmp/"
  save-place-file (concat user-temporary-file-directory "saveplace")
  history-length t
  color-theme-is-global t
  auto-save-list-file-prefix (concat
                              user-temporary-file-directory ".auto-saves-")
  auto-save-file-name-transforms `((".*" ,user-temporary-file-directory t))
  inhibit-startup-message t
  delete-old-versions t)

(make-directory user-temporary-file-directory t)

(defconst use-backup-dir t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))

(defun reload-dot-emacs()
  (interactive)
  (if(bufferp (get-file-buffer ".emacs"))
      (save-buffer(get-buffer ".emacs")))
  (load-file "~/.emacs.d/init.el")
  (message ".emacs reloaded successfully"))

;(setq server-socket-dir "~/.tmp/socket")
;(setq server-use-tcp t)
(server-start)
