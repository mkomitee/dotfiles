; Emacs now has a good editor.

(req-package evil
  :require undo-tree
  :pre-load (setq evil-want-C-u-scroll t
                  evil-want-C-w-in-emacs-state t
                  evil-want-fine-undo nil
                  evil-want-fine-undo nil
                  )
  :config (progn
            (setq evil-emacs-state-cursor '("red" box)
                  evil-normal-state-cursor '("white" box)
                  evil-insert-state-cursor '("white" bar)
                  evil-backspace-join-lines t
                  evil-magic (quote very-magic)
                  evil-search-module (quote evil-search)
                  )
            (evil-mode 1)

            ;; Update modes, everything that defaults to emacs state should
            ;; instead default to motion state. Anything that requires editing,
            ;; we'll remove from motion state so it's in notmal mode by default,
            ;; unless there's a VERY good reason to use emacs state.
            (setq evil-motion-state-modes (append evil-emacs-state-modes
                                                  evil-motion-state-modes)
                  evil-emacs-state-modes nil
                  evil-motion-state-modes (remove 'Custom-mode evil-motion-state-modes)
                  )

            ;; This shouldn't be necessary, help-mode is already in
            ;; evil-motion-state-modes, but for some reason the first
            ;; time I enter it, it's evil-state is nil.
            (add-hook 'help-mode-hook 'evil-motion-state)

            (add-to-list 'evil-motion-state-modes 'package-menu-mode)
            (evil-add-hjkl-bindings package-menu-mode-map 'motion
              "H" 'package-menu-quick-help)
            )
            ;; This shouldn't be necessary, package-menu-mode is
            ;; already in evil-motion-state-modes, but for some reason
            ;; the first time I enter it, it's evil-state is
            ;; nil.
            (add-hook 'package-menu-mode-hook 'evil-motion-state)

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

              (kbd "<down>") 'shrink-window
              (kbd "<up>") 'enlarge-window
              (kbd "<right>") 'enlarge-window-horizontally
              (kbd "<left>") 'shrink-window-horizontally

              "0" 'smarter-move-beginning-of-line


              ;; I switch ' and ` in vim, so I do so here as well
              "'" 'evil-goto-mark
              "`" 'evil-goto-mark-line
              )

            (evil-define-key 'normal global-map
              "Y" (kbd "y$")
              )

            (define-key evil-window-map (kbd "<left>") 'winner-undo)
            (define-key evil-window-map (kbd "<right>") 'winner-redo)

            ;; Easier window navigation. Note, this kills the C-h help-map prefix,
            ;; which is why I replicate most of that functionality in my leader-map.
            (define-key global-map "\C-j" 'evil-window-down)
            (define-key global-map "\C-k" 'evil-window-up)
            (define-key global-map "\C-h" 'evil-window-left)
            (define-key global-map "\C-l" 'evil-window-right)

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
            (global-set-key [escape] 'evil-exit-emacs-state)
            )
  )

(req-package help-fns+
  :require evil-leader
  :config (progn
            (evil-leader/set-key
              "hdc" 'describe-command
              "hdo" 'describe-option
              )
            )
  )

(req-package evil-leader
  :require evil
  :config (progn
            (evil-leader/set-leader "<SPC>")
            (global-evil-leader-mode)
            (evil-leader/set-key
              "u" 'universal-argument

              "hdp" 'describe-package
              "hdf" 'describe-function
              "hdb" 'describe-bindings
              "hdim" 'describe-input-method
              "hdk" 'describe-key
              "hdle" 'describe-language-environment
              "hdM" 'describe-mode
              "hdm" 'describe-minor-mode
              "hds" 'describe-syntax
              "hdv" 'describe-variable

              "hac" 'apropos-command
              "had" 'apropos-documentation
              "hdlh" 'display-local-help
              "hfh" 'help-for-help
              "hils" 'info-lookup-symbol

              "hca" 'customize-apropos
              "hcf" 'customize-face
              "hcg" 'customize-group
              "hcm" 'customize-mode
              "hct" 'customize-themes
              "hcv" 'customize-variable

              "w" 'evil-window-map
              )
            )
  )

(req-package evil-args
  :require evil
  :config (progn
            (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
            (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
            (evil-define-key 'motion global-map
              (kbd "M-l") 'evil-forward-arg
              (kbd "M-h") 'evil-backward-arg
              (kbd "M-k") 'evil-jump-out-args
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
  :config (global-evil-snipe-mode 1)
  )

(req-package evil-surround
  :require evil
  :config (global-evil-surround-mode 1)
  )

(req-package evil-tabs
  :require evil
  :config (global-evil-tabs-mode)
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
  :require evil evil-leader
  :config (progn
            (evil-leader/set-key
              "eg" 'evil-execute-in-god-state
              )
            (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
            )
  )

(req-package evil-visual-mark-mode
  :require evil
  :config (evil-visual-mark-mode)
  )


(req-package evil-search-highlight-persist
  :require evil
  :config (evil-search-highlight-persist)
  )
