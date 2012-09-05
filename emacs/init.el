;; Enabling to gain access to the emacs-starter-kit because I don't
;; know what I'm doing. https://github.com/technomancy/emacs-starter-kit/
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; These are the packages I wabnt installed
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      evil
                      textmate
                      color-theme
                      ack-and-a-half
                      fill-column-indicator
                      markdown-mode
                      color-theme-molokai
                      puppet-mode
                      python-mode
                      )
  "A list of packages to ensure are installed at launch.")

;; This ensures they're installed
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; add our additional libraries to our  load path
(add-to-list 'load-path "~/.dotfiles/emacs/lib")

;; Enable line numbers
(global-linum-mode t)

;; Color-column
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkred")
(add-hook 'after-change-major-mode-hook 'fci-mode)
(add-hook 'after-change-major-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (setq fci-rule-column 80)))

(require 'textmate)
(textmate-mode)

;; No idea what a good emacs theme is so using molokai which is decent
(require 'color-theme)
(color-theme-molokai)

;; create some aliases for ack
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Enable flymake/pylint, note this only works when epylint is in my
;; path, which isn't the case at home when started with spotlight. It
;; has to be started from the commandline.
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

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

(allow-line-as-region-for-function comment-or-uncomment-region)

;; This enables vim compatible bindings, ...
(setq evil-default-state 'normal)
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; emulates surround.vim
(require 'surround)
(global-surround-mode 1)

(define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map [escape] 'keyboard-escape-quit)

(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)
(define-key evil-normal-state-map "\C-c" 'delete-window)
(define-key evil-normal-state-map "|" (kbd ":vsplit C-m C-l"))
(define-key evil-normal-state-map "_" (kbd ":split C-m C-j"))
(define-key evil-normal-state-map " d" 'speedbar)
(define-key evil-normal-state-map " b" 'ido-display-buffer)
(define-key evil-normal-state-map " p" 'textmate-goto-file)
(define-key evil-normal-state-map " ev" (kbd ":e ~/.dotfiles/emacs/init.el"))
(define-key evil-normal-state-map "  " 'comment-or-uncomment-region-or-line)
(define-key evil-visual-state-map "  " 'comment-or-uncomment-region-or-line)

;; Since there's no 'noremap' functionality available, I have to first
;; define a sequence of characters to perform the shift, and THEN
;; remap > and < to call that other sequence and then gv, this works
;; but it makes me sad.
(define-key evil-visual-state-map "g>" 'evil-shift-right)
(define-key evil-visual-state-map "g<" 'evil-shift-left)
(define-key evil-visual-state-map ">" (kbd "g>gv"))
(define-key evil-visual-state-map "<" (kbd "g<gv"))

;; I switch ' and ` in vim, so I do so here as well
(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)

;; Here's how to define a new ex command
(defun evil-ex-define-cmd "ack" 'ack)
(defun evil-ex-define-cmd "eshell" 'eshell)
;; TODO: get something like this working
;; (defun evil-ex-define-cmd "sort" 'sort-lines)

;; We prefer normal mode in several places
(defvar my-normal-modes'(
                         package-menu-mode
                         )
  "List of modes to put in normal mode by default, despite evil defaults")

;; This ensures they're installed
(dolist (p my-normal-modes)
  (delete p 'evil-emacs-state-modes)
  (add-to-list 'evil-normal-state-modes p))

;; Don't litter auto-save turds all over the file system
(setq backup-directory-alist
     `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
     `((".*" ,temporary-file-directory t)))

(eval-after-load "dired"
  '(require 'dired-x))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

(server-start)
