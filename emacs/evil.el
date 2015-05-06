;; Emacs now has a good editor.

(req-package evil
  :require undo-tree
  :init (setq evil-want-C-u-scroll t
              evil-want-C-w-in-emacs-state t
              evil-want-fine-undo nil
              evil-want-fine-undo nil
              )
  :config (progn
            (setq evil-emacs-state-cursor '("red" box)
                  evil-normal-state-cursor '("#586e75" box)
                  evil-insert-state-cursor '("#586e75" bar)
                  evil-backspace-join-lines t
                  evil-magic 'very-magic
                  evil-search-module 'evil-search
                  )

            (evil-mode 1)

            (dolist (mode '(Custom-mode))
              (evil-set-initial-state mode 'normal))

            (dolist (mode '(package-menu-mode
                            messages-buffer-mode))
              (evil-set-initial-state mode 'motion))

            (evil-define-key 'motion package-menu-mode-map
              "H" 'package-menu-quick-help)

            (evil-define-key 'normal dired-mode-map
              "h" 'describe-mode
              "l" 'dired-do-redisplay)

            (evil-define-key 'motion global-map
              "[b" 'evil-prev-buffer
              "]b" 'evil-next-buffer
              "[w" 'evil-window-prev
              "]w" 'evil-window-next
              "[e" 'previous-error
              "]e" 'next-error
              "]s" 'flyspell-goto-next-error

              "|" 'komitee/split-horizontally
              "_" 'komitee/split-vertically

              "j" 'evil-next-visual-line
              "k" 'evil-previous-visual-line

              ;; I switch ' and ` in vim, so I do so here as well
              "'" 'evil-goto-mark
              "`" 'evil-goto-mark-line
              )

            (evil-define-key 'normal global-map
              "Y" (kbd "y$")
              )

            (define-key evil-window-map (kbd "<backspace>") 'winner-undo)
            (define-key evil-window-map (kbd "<S-backspace>") 'winner-redo)
            (define-key evil-window-map (kbd "<down>") 'shrink-window)
            (define-key evil-window-map (kbd "<up>") 'enlarge-window)
            (define-key evil-window-map (kbd "<right>") 'enlarge-window-horizontally)
            (define-key evil-window-map (kbd "<left>") 'shrink-window-horizontally)
            (define-key evil-window-map "|" 'komitee/split-horizontally)
            (define-key evil-window-map "_" 'komitee/split-vertically)
            (define-key evil-window-map "f" 'text-scale-adjust)

            ;; Easier window navigation. Note, this kills the C-h help-map prefix,
            ;; which is why I replicate most of that functionality in my leader-map.
            (define-key global-map "\C-j" 'evil-window-down)
            (define-key global-map "\C-k" 'evil-window-up)
            (define-key global-map "\C-h" 'evil-window-left)
            (define-key global-map "\C-l" 'evil-window-right)

            ;; C-h is the default map for this, but it's really useful
            ;; so we'll re-add it using Meta
            (define-key global-map "\M-h" 'help-command)

            (evil-define-key 'visual global-map
              ;; There's probably an easier way to do this by defining a function,
              ;; but I can't figure it out. It re-selects the shifted region after
              ;; the shift.
              (kbd "C->") 'evil-shift-right
              ">" (kbd "C-> gv")
              (kbd "C-<") 'evil-shift-left
              "<" (kbd "C-< gv")

              )

            ;; Here's how to define a new ex command
            (evil-ex-define-cmd "Q" 'evil-quit)
            (evil-ex-define-cmd "QA" 'evil-quit-all)
            (evil-ex-define-cmd "Qa" 'evil-quit-all)
            (evil-ex-define-cmd "WQ" 'evil-save-and-close)
            (evil-ex-define-cmd "Wq" 'evil-save-and-close)
            (evil-ex-define-cmd "esh[ell]" 'eshell)

            ;; escape quits
            (define-key evil-normal-state-map [escape] 'komitee/nohl-quit)
            (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

            (define-key evil-motion-state-map [down-mouse-1] nil)
            (define-key evil-visual-state-map [mouse-2] nil)
            (define-key evil-normal-state-map [mouse-2] nil)
            (define-key evil-insert-state-map [mouse-2] nil)
            )
  )

(req-package help-fns+)

(req-package evil-leader
  :require evil
  :config (progn
            (evil-leader/set-leader "<SPC>")
            (global-evil-leader-mode)
            (evil-leader/set-key
              "u" 'universal-argument

              ;; This is nice, but it doesn't help us when we're in
              ;; insert-state or an emacs-state.
              "h" 'help-command

              "ca" 'customize-apropos
              "cf" 'customize-face
              "cg" 'customize-group
              "cm" 'customize-mode
              "ct" 'customize-themes
              "cv" 'customize-variable

              "w" 'evil-window-map

              "ss" 'just-one-space
              "x" 'helm-M-x

              "bd" 'evil-delete-buffer
              )
            )
  )

(req-package evil-commentary
  :diminish evil-commentary-mode
  :require evil
  :config (evil-commentary-default-setup)
  )

(req-package evil-easymotion
  :require evil
  :config (evilem-default-keybindings "M-SPC")
  )

(req-package evil-indent-textobject
  :require evil
  )

(req-package evil-jumper
  :require evil
  :config (global-evil-jumper-mode)
  )

(req-package evil-matchit
  :require evil
  :config (global-evil-matchit-mode 1)
  )

(req-package evil-snipe
  :require evil
  :diminish evil-snipe-mode
  :config (global-evil-snipe-mode 1)
  )

(req-package evil-surround
  :require evil
  :config (global-evil-surround-mode 1)
  )

(req-package evil-visualstar
  :require evil
  )

(req-package evil-numbers
  :require evil
  :config (evil-define-key 'motion global-map
            (kbd "C-c +") 'evil-numbers/inc-at-pt
            (kbd "C-c -") 'evil-numbers/dec-at-pt
            )
  )

(req-package evil-exchange
  :require evil
  :config (evil-exchange-install))


(req-package evil-god-state
  :require (evil evil-leader)
  :config (progn
            (evil-leader/set-key
              "eg" 'evil-execute-in-god-state
              )
            (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
            )
  )

(req-package evil-search-highlight-persist
  :require evil
  :config (evil-search-highlight-persist)
  )

(req-package evil-anzu
  :diminish anzu-mode
  :require evil
  :config (global-anzu-mode 1)
  )
