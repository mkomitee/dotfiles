;;;; This code integrates Viper with the outside world

(require 'vimpulse-utils)
(require 'vimpulse-viper-function-redefinitions)

;;; undo-tree.el

(when (and (boundp 'undo-tree-visualizer-map)
           (fboundp 'undo-tree-visualizer-quit))

  (defadvice undo-tree-visualize (after vimpulse activate)
    "Enable Viper."
    (viper-mode))

  (defun vimpulse-undo-quit ()
    "Quit the undo-tree visualizer and delete window."
    (interactive)
    (let ((w (selected-window)))
      (undo-tree-visualizer-quit)
      (when (eq (selected-window) w)
        (delete-window))))

  (define-key undo-tree-visualizer-map [remap viper-backward-char] 'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-map [remap viper-forward-char] 'undo-tree-visualize-switch-branch-right)
  (define-key undo-tree-visualizer-map [remap viper-next-line] 'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-map [remap viper-previous-line] 'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-map [remap undo-tree-visualizer-scroll-left] 'viper-scroll-up)
  (define-key undo-tree-visualizer-map [remap undo-tree-visualizer-scroll-left] 'viper-scroll-up-one)
  (define-key undo-tree-visualizer-map [remap undo-tree-visualizer-scroll-right] 'viper-scroll-down)
  (define-key undo-tree-visualizer-map [remap undo-tree-visualizer-scroll-right] 'viper-scroll-down-one)
  (define-key undo-tree-visualizer-map [remap viper-intercept-ESC-key] 'vimpulse-undo-quit)
  (define-key undo-tree-visualizer-map [remap viper-nil] 'vimpulse-undo-quit)
  (define-key undo-tree-visualizer-map [remap undo-tree-visualizer-quit] 'vimpulse-undo-quit)
  (define-key undo-tree-visualizer-map [remap viper-next-line-at-bol] 'vimpulse-undo-quit)

  (add-to-list 'viper-vi-state-mode-list 'undo-tree-visualizer-mode)

  (add-to-list 'ex-token-alist '("undolist" (undo-tree-visualize)))
  (add-to-list 'ex-token-alist '("ulist" (undo-tree-visualize))))

;;; Isearch

(defcustom vimpulse-incremental-search t
  "Use isearch for / and ?, on by default."
  :type 'boolean
  :group 'vimpulse)

(defcustom vimpulse-flash-delay 2
  "Number of seconds to flash search matches."
  :type 'integer
  :group 'vimpulse)

(defvar vimpulse-flash-timer nil
  "Timer for flashing search results.")

(defadvice isearch-message-prefix (around vimpulse-search activate)
  "Use vi prefix if appropriate."
  (if vimpulse-search-prompt
      (setq ad-return-value vimpulse-search-prompt)
    ad-do-it))

(defadvice isearch-delete-char (around vimpulse-search activate)
  "Exit search if no search string."
  (if (and vimpulse-search-prompt
           (string= isearch-string ""))
      (isearch-exit)
    ad-do-it))

(defvar viper-re-search)
(defvar viper-s-forward)
(defvar viper-s-string)

(defadvice isearch-update-ring (after vimpulse-search activate)
  "Update `viper-s-string'."
  (when (eq viper-re-search regexp)
    (setq viper-s-string string)))

(defadvice isearch-lazy-highlight-search (around vimpulse-search activate)
  "Deactivate `viper-search-wrap-around'."
  (let (viper-search-wrap-around)
    ad-do-it))

(defadvice viper-search (after vimpulse-search activate)
  "Update isearch history."
  (isearch-update-ring string viper-re-search))

;; if `viper-search-wrap-around' is t, we want the search to wrap
(defun vimpulse-search-fun-function ()
  "Return a wrapping search function.
Based on `viper-re-search' and `viper-s-forward'."
  `(lambda (regexp &optional bound noerror count)
     (let ((orig (point))
           (search-fun (if isearch-regexp
                           (if isearch-forward
                               're-search-forward
                             're-search-backward)
                         (if isearch-forward
                             'search-forward
                           'search-backward)))
           retval)
       (setq retval (funcall search-fun regexp bound t count))
       (when (and (not retval) viper-search-wrap-around)
         (goto-char (if isearch-forward (point-min) (point-max)))
         (setq retval (funcall search-fun regexp bound t count))
         (unless retval
           (goto-char orig)))
       retval)))

(defun vimpulse-search-backward (arg)
  "Search backward for user-entered text.
Searches for regular expression if `viper-re-search' is t."
  (interactive "P")
  (let ((vimpulse-search-prompt "?")
        (lazy-highlight-initial-delay 0)
        (orig (point))
        (isearch-mode-map isearch-mode-map)
        (isearch-search-fun-function 'vimpulse-search-fun-function)
        (oldmsg (current-message))
        message-log-max
        search-nonincremental-instead)
    (vimpulse-vi-remap 'viper-intercept-ESC-key
                       'isearch-exit
                       isearch-mode-map)
    (setq viper-s-forward nil)
    (isearch-backward viper-re-search)
    (when (and (eq orig (point))
               (not (string= isearch-string "")))
      (isearch-repeat-backward)
      (isearch-exit))
    (if oldmsg (message "%s" oldmsg)
      (message nil))
    (unless (string= isearch-string "")
      (vimpulse-flash-search-pattern t))
    (setq vimpulse-this-motion 'viper-search-next)))

(put 'vimpulse-search-backward 'function-documentation
     (format "Search backward for user-entered text.
Searches for regular expression if `viper-re-search' is t.

%s" (if (and (fboundp 'isearch-forward)
             (documentation 'isearch-forward))
        (format "Below is the documentation string for `isearch-forward',
which lists available keys:

%s" (documentation 'isearch-forward)))))

(defun vimpulse-search-forward (arg)
  "Search forward for user-entered text.
Searches for regular expression if `viper-re-search' is t."
  (interactive "P")
  (let ((vimpulse-search-prompt "/")
        (orig (point))
        (isearch-mode-map isearch-mode-map)
        (isearch-search-fun-function 'vimpulse-search-fun-function)
        (oldmsg (current-message))
        message-log-max
        search-nonincremental-instead)
    (vimpulse-vi-remap 'viper-intercept-ESC-key
                       'isearch-exit
                       isearch-mode-map)
    (setq viper-s-forward t)
    (isearch-forward viper-re-search)
    (and isearch-other-end (goto-char isearch-other-end))
    (when (and (eq orig (point))
               (not (string= isearch-string "")))
      (isearch-repeat-forward)
      (isearch-exit))
    (and isearch-other-end (goto-char isearch-other-end))
    (if oldmsg (message "%s" oldmsg)
      (message nil))
    (unless (string= isearch-string "")
      (vimpulse-flash-search-pattern t))
    (setq vimpulse-this-motion 'viper-search-next)))

(put 'vimpulse-search-forward 'function-documentation
     (format "Search forward for user-entered text.
Searches for regular expression if `viper-re-search' is t.

%s" (if (and (fboundp 'isearch-forward)
             (documentation 'isearch-forward))
        (format "Below is the documentation string for `isearch-forward',
which lists available keys:

%s" (documentation 'isearch-forward)))))

(defun vimpulse-flash-search-pattern (&optional only-current)
  "Flash search matches for duration of `vimpulse-flash-delay'."
  (let ((lazy-highlight-initial-delay 0)
        (isearch-search-fun-function 'vimpulse-search-fun-function)
        (isearch-case-fold-search case-fold-search)
        (disable (lambda (&optional arg) (vimpulse-flash-hook t))))
    (when vimpulse-flash-timer
      (if (fboundp 'disable-timeout)
          (disable-timeout vimpulse-flash-timer)
        (cancel-timer vimpulse-flash-timer)))
    (when (viper-has-face-support-p)
      (isearch-highlight (match-beginning 0) (match-end 0))
      (unless only-current
        (setq isearch-string viper-s-string
              isearch-forward viper-s-forward
              isearch-regexp viper-re-search
              isearch-lazy-highlight-wrapped nil
              isearch-lazy-highlight-start (point)
              isearch-lazy-highlight-end (point))
        (and (fboundp 'isearch-lazy-highlight-new-loop)
             (isearch-lazy-highlight-new-loop))
        (unless (and (boundp 'isearch-lazy-highlight-overlays)
                     isearch-lazy-highlight-overlays)
          (and (fboundp 'isearch-lazy-highlight-update)
               (isearch-lazy-highlight-update))))
      (add-hook 'pre-command-hook 'vimpulse-flash-hook)
      (add-hook 'pre-command-hook 'vimpulse-clean-isearch-overlays)
      (setq vimpulse-flash-timer
            (if (fboundp 'run-at-time)
                (add-timeout vimpulse-flash-delay disable nil)
              (run-at-time vimpulse-flash-delay nil disable))))))

(defun vimpulse-clean-isearch-overlays ()
  "Cleans isearch overlays if `this-command' is not search."
  (remove-hook 'pre-command-hook 'vimpulse-clean-isearch-overlays)
  (when (not (memq this-command
		   '(viper-exec-mapped-kbd-macro
		     viper-search
		     viper-search-backward
		     viper-search-forward
		     viper-search-next
		     viper-search-Next
		     vimpulse-search-backward
		     vimpulse-search-forward
		     vimpulse-search-backward-for-symbol-at-point
		     vimpulse-search-forward-for-symbol-at-point)))
    (isearch-clean-overlays)))

(defun vimpulse-flash-hook (&optional force)
  "Disable hightlighting if `this-command' is not search.
Disable anyway if FORCE is t."
  (when (or force
            ;; to avoid flicker, don't disable highlighting if the
            ;; next command is also a search command
            (not (memq this-command
                       '(viper-exec-mapped-kbd-macro
                         viper-search
                         viper-search-backward
                         viper-search-forward
                         viper-search-next
                         viper-search-Next
                         vimpulse-search-backward
                         vimpulse-search-forward
                         vimpulse-search-backward-for-symbol-at-point
                         vimpulse-search-forward-for-symbol-at-point))))
    (isearch-dehighlight)
    (setq isearch-lazy-highlight-last-string nil)
    (and (fboundp 'isearch-highlight-all-cleanup)
         (isearch-highlight-all-cleanup))
    (and (fboundp 'lazy-highlight-cleanup)
         (lazy-highlight-cleanup t))
    (when vimpulse-flash-timer
      (cancel-timer vimpulse-flash-timer)))
  (remove-hook 'pre-command-hook 'vimpulse-flash-hook))

(when vimpulse-incremental-search
  (defvaralias 'viper-case-fold-search 'case-fold-search)
  (defalias 'viper-search-backward 'vimpulse-search-backward)
  (defalias 'viper-search-forward 'vimpulse-search-forward)
  (defalias 'viper-flash-search-pattern 'vimpulse-flash-search-pattern))

;;; Add vi navigation to help buffers

;; Apropos
(eval-after-load 'apropos
  '(when vimpulse-want-vi-keys-in-apropos
     (add-to-list 'viper-vi-state-mode-list 'apropos-mode)
     (let ((map (copy-keymap apropos-mode-map)))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'apropos-mode 'vi-state map))))

;; Buffer-menu
(eval-after-load "buff-menu"
  '(when vimpulse-want-vi-keys-in-buffmenu
     (setq viper-emacs-state-mode-list
           (delq 'Buffer-menu-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'Buffer-menu-mode)
     (let ((map (copy-keymap Buffer-menu-mode-map)))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'Buffer-menu-mode 'vi-state map))))

;; Info
(eval-after-load 'info
  '(when vimpulse-want-vi-keys-in-Info
     (setq viper-emacs-state-mode-list
           (delq 'Info-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'Info-mode)
     (let ((map (copy-keymap Info-mode-map)))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (define-key map "\C-t" 'Info-history-back) ; l
       (define-key map "\C-o" 'Info-history-back)
       (define-key map (kbd "\M-h") 'Info-help) ; h
       (define-key map " " 'Info-scroll-up)
       (define-key map "\C-]" 'Info-follow-nearest-node)
       (define-key map [backspace] 'Info-scroll-down)
       (viper-modify-major-mode 'Info-mode 'vi-state map))))

;; Help
(eval-after-load 'help-mode
  '(when vimpulse-want-vi-keys-in-help
     (setq viper-emacs-state-mode-list
           (delq 'help-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'help-mode)
     (let ((map (copy-keymap help-mode-map)))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (define-key map "q" 'View-quit)
       (viper-modify-major-mode 'help-mode 'vi-state map))))

;; Slime
(eval-after-load 'slime
  '(defadvice slime-popup-buffer-mode (after vimpulse activate)
     (when slime-popup-buffer-mode
       (viper-add-local-keys
        'vi-state '(([?q] . slime-popup-buffer-quit-function))))))

;;; Edebug

(eval-after-load 'edebug
  '(progn
     (define-key edebug-mode-map [remap viper-forward-char] 'edebug-step-mode)
     (define-key edebug-mode-map [remap viper-search-next] 'edebug-next-mode)
     (define-key edebug-mode-map [remap vimpulse-goto-first-line] 'edebug-go-mode)
     (define-key edebug-mode-map [remap viper-goto-line] 'edebug-Go-nonstop-mode)
     (define-key edebug-mode-map [remap viper-goto-char-forward] 'edebug-trace-mode)
     (define-key edebug-mode-map [remap viper-goto-char-backward] 'edebug-Trace-fast-mode)
     (define-key edebug-mode-map [remap vimpulse-change] 'edebug-continue-mode)
     (define-key edebug-mode-map [remap viper-change-to-eol] 'edebug-Continue-fast-mode)
     (define-key edebug-mode-map [remap viper-find-char-forward] 'edebug-forward-sexp)
     (define-key edebug-mode-map [remap viper-backward-char] 'edebug-goto-here)
     (define-key edebug-mode-map [remap viper-Insert] 'edebug-instrument-callee)
     (define-key edebug-mode-map [remap viper-insert] 'edebug-step-in)
     (define-key edebug-mode-map [remap viper-open-line] 'edebug-step-out)
     (define-key edebug-mode-map [remap viper-nil] 'top-level)
     (define-key edebug-mode-map [remap viper-query-replace] 'edebug-top-level-nonstop)
     (define-key edebug-mode-map [remap viper-append] 'abort-recursive-edit)
     (define-key edebug-mode-map [remap viper-substitute-line] 'edebug-stop)
     (define-key edebug-mode-map [remap viper-backward-word] 'edebug-set-breakpoint)
     (define-key edebug-mode-map [remap undo-tree-undo] 'edebug-unset-breakpoint)
     (define-key edebug-mode-map [remap viper-backward-Word] 'edebug-next-breakpoint)
     (define-key edebug-mode-map [remap viper-delete-char] 'edebug-set-conditional-breakpoint)
     (define-key edebug-mode-map [remap viper-delete-backward-char] 'edebug-set-global-break-condition)
     (define-key edebug-mode-map [remap vimpulse-replace] 'edebug-previous-result)
     (define-key edebug-mode-map [remap viper-end-of-word] 'edebug-eval-expression)
     (define-key edebug-mode-map [remap viper-end-of-Word] 'edebug-visit-eval-list)
     (define-key edebug-mode-map [remap vimpulse-visual-toggle-char] 'edebug-view-outside)
     (define-key edebug-mode-map [remap viper-put-back] 'edebug-bounce-point)
     (define-key edebug-mode-map [remap viper-Put-back] 'edebug-view-outside)
     (define-key edebug-mode-map [remap viper-forward-Word] 'edebug-toggle-save-windows)
     (define-key edebug-mode-map [remap vimpulse-search-backward] 'edebug-help)
     (define-key edebug-mode-map [remap vimpulse-delete] 'edebug-backtrace)
     (define-key edebug-mode-map [remap viper-previous-line-at-bol] 'negative-argument)
     (define-key edebug-mode-map [remap vimpulse-indent] 'edebug-temp-display-freq-count)))

;;; ElDoc

(eval-after-load 'eldoc
  '(apply 'eldoc-add-command
          (append vimpulse-viper-movement-cmds
                  vimpulse-core-movement-cmds)))

;;; Folding

(eval-after-load 'hideshow
  '(progn
     (defun vimpulse-za ()
       (interactive)
       (hs-toggle-hiding)
       (hs-hide-level vimpulse-fold-level))
     (defun vimpulse-hs-setup ()
       (define-key viper-vi-basic-map "za" 'vimpulse-za)
       (define-key viper-vi-basic-map "zm" 'hs-hide-all)
       (define-key viper-vi-basic-map "zr" 'hs-show-all)
       (define-key viper-vi-basic-map "zo" 'hs-show-block)
       (define-key viper-vi-basic-map "zc" 'hs-hide-block))
     (add-hook 'hs-minor-mode-hook 'vimpulse-hs-setup)))

(provide 'vimpulse-compatibility)
