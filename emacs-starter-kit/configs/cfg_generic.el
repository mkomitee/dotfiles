;;; Themes, ...
(require 'color-theme-ir-black)
(color-theme-ir-black)

;;; Line numbers, ...
(setq linum-format "%5.d ")
(global-linum-mode 1)

;;; Setup spaces/tabs
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq c-basic-indent 4)

;;; Only first learning emacs, use viper as a crutch
(setq viper-mode t)
(setq viper-custom-file-name "~/.emacs.d/viper")
(require 'viper)
(require 'vimpulse)
(vimpulse-vmap ",c" 'comment-dwim)

;; alter default behavior
(prefer-coding-system 'utf-8) ;; use utf-8

(require 'saveplace)
(setq
  user-temporary-file-directory "~/.tmp/"
  save-place-file (concat user-temporary-file-directory "saveplace")
  column-number-mode t
  history-length t
  indicate-empty-lines t
  color-theme-is-global t
  auto-save-list-file-prefix (concat user-temporary-file-directory ".auto-saves-")
  auto-save-file-name-transforms `((".*" ,user-temporary-file-directory t))
  inhibit-startup-message t
  version-control t
  delete-old-versions t)

(menu-bar-mode -1)
(show-paren-mode +1)
(when (featurep 'x) ;; when its gui..
  (tool-bar-mode -1) ;; hide the excess chrome
  (scroll-bar-mode -1)) ;; and turn off scrollbars
(show-paren-mode +1) ;; show paired parenthasis
(auto-compression-mode +1) ;; auto compress/decompress files
(auto-fill-mode +1)
(icomplete-mode +1) ;; incremental minibuffer completion
(global-font-lock-mode +1) ;; make pretty fonts?
(when (or
       (string= "mac" window-system)
       (string= "ns" window-system))
  ;; Mac-Specific Settings
  (set-default-font
   ;; To get a font list, type xfontsel in console when the X11 server is running
   "-*-Monaco-normal-r-*-*-12-102-120-120-c-*-iso8859-1")
  (setq mac-command-modifier 'meta))

;; (when (string= "x" window-system)
;;   ;; Linux-Specific Settings
;;   (set-default-font
;;    "-unknown-Envy Code R-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))
(setq-default
 truncate-lines t ;; truncate lines, not wrap
 save-place t ;; enable save-place globally by default
 indent-tabs-mode nil) ;; indent via spaces not tabs

;; (toggle-debug-on-error t) ;; show traceback on error
(fset 'yes-or-no-p 'y-or-n-p) ;; allows you to type "y" instead of "yes" on exit
(mouse-avoidance-mode 'cat-and-mouse) ;; mouse jumps away when typing under it
(if (load "mwheel" t)
    (mwheel-install)) ;; turn on the mouse wheel

;; ;; enable windmove if the package is available
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; stop leaving # files and ~ files strewn about. put them in a temp folder
(make-directory user-temporary-file-directory t)
(defconst use-backup-dir t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))

;; ;; One of Apps
(require 'textmate) ;; defunkt's textmate.el
(textmate-mode)
(require 'undo-tree)
(global-undo-tree-mode t)
(winner-mode t) ;; turn on saved buffer configs
(require 'smooth-scrolling) ;; stop text from jumping on scroll.
(require 'column-marker)
(require 'gist)
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(require 'etags-table)
(require 'etags-select)
(autoload 'scratch "scratch" nil t)
(require 'framemove)
(setq framemove-hook-into-windmove t)
;; (require 'ectags)

(defun reload-dot-emacs()
  (interactive)
  (if(bufferp (get-file-buffer ".emacs"))
      (save-buffer(get-buffer ".emacs")))
  (load-file "~/.emacs.d/init.el")
  (message ".emacs reloaded successfully"))

