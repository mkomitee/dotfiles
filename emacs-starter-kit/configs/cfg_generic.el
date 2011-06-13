;;; Themes, ...
(require 'color-theme-ir-black)
(color-theme-ir-black)

;;; Line numbers with a sensible format, ...
(setq linum-format "%5.d ")
(global-linum-mode 1)

(require 'yasnippet)
(require 'highlight-80+)
(require 'fic-mode)

;; Setup super comment mode. It will comment or uncomment selected text, or the
;; current line if there is no selection. Original idea from
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

;;; Setup spaces/tabs. Apparently this is a lot more complicated than
;;; I think, so I'll have to do a _LOT_ more to get this working how I want.
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)

(setq speedbar-tag-split-minimum-length 100) ; Allow upto 100 tags for
                                        ; a file in speedbar before it
                                        ; splits them up

;; Setup undo/redo with C-z C-S-z mappings
(require 'undo-tree)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
(global-set-key (kbd "C-x b") 'lusty-buffer-explorer)

;; (delete-selection-mode 1) ; typing with a highlighted selection
                          ; replaces the text in that selection
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; Try to avoid littering the filesystem with random types of
;; tempfiles and save data. This is by no means complete.
(setq
 user-temporary-file-directory "~/.tmp/"
  save-place-file (concat user-temporary-file-directory "saveplace")
  auto-save-list-file-prefix (concat
                              user-temporary-file-directory ".auto-saves-")
  auto-save-file-name-transforms `((".*" ,user-temporary-file-directory t))
  delete-old-versions t)

(defconst use-backup-dir t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))

(make-directory user-temporary-file-directory t)

(setq
 history-length t
 color-theme-is-global t
 inhibit-startup-message t)


(defun reload-dot-emacs()
  (interactive)
  (if(bufferp (get-file-buffer ".emacs"))
      (save-buffer(get-buffer ".emacs")))
  (load-file "~/.emacs.d/init.el")
  (message ".emacs reloaded successfully"))

(setq kill-whole-line t) ; When we kill-line, we should also kill the
                         ; newline that terminates the line.

; After we yank a block of text into existance, we should indent it.
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode lisp-mode c-mode c++-mode
                                           ruby-mode cperl-mode
                                           python-mode))
      (indent-region (region-beginning) (region-end) nil)))

;;(server-start)

(load "server")
(setq server-use-tcp t)
(unless (server-running-p) (server-start))

(defun my-done ()
  (interactive)
  (server-edit)
  (make-frame-invisible nil t))

(if (server-running-p)
    (global-set-key (kbd "C-x h") 'my-done))

(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
