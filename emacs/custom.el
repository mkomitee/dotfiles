(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/.backups/" t))))
 '(auto-save-list-file-prefix "~/.emacs.d/.auto-save-list/.saves-")
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(company-idle-delay nil)
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e"
           "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832"
           default)))
 '(eshell-aliases-file (concat komitee/emacs-config-directory "aliases"))
 '(eshell-directory-name "~/")
 '(evil-backspace-join-lines t)
 '(evil-leader/leader "<SPC>")
 '(evil-magic (quote very-magic))
 '(evil-search-module (quote evil-search))
 '(evil-want-C-u-scroll t)
 '(evil-want-C-w-in-emacs-state t)
 '(evil-want-fine-undo nil)
 '(fci-rule-character 9550)
 '(fci-rule-color "darkred")
 '(fci-rule-column 80)
 '(frame-background-mode (quote dark))
 '(guide-key/idle-delay 0.5)
 '(guide-key/popup-window-position (quote bottom))
 '(ido-save-directory-list-file "~/.emacs.d/.ido.last")
 '(ido-use-filename-at-point (quote guess))
 '(indicate-buffer-boundaries (quote left))
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice (quote remember-notes))
 '(linum-format "%4d ")
 '(package-archives
   (quote
    (("melpa" . "http://melpa.milkbox.net/packages/")
     ("org" . "http://orgmode.org/elpa/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("elpy" . "http://jorgenschaefer.github.io/packages/"))))
 '(projectile-cache-file "~/.emacs.d/.projectile.cache")
 '(projectile-known-projects-file "~/.emacs.d/.projectile-bookmarks.eld")
 '(projectile-require-project-root nil)
 '(python-fill-docstring-style (quote django))
 '(python-shell-interpreter "ipython")
 '(recentf-max-menu-items 50)
 '(recentf-max-saved-items 100)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(remember-notes-initial-major-mode (quote rst-mode))
 '(savehist-additional-variables (quote (search ring regexp-search-ring)))
 '(savehist-autosave-interval 60)
 '(savehist-file "~/.emacs.d/.savehist")
 '(scroll-conservatively 9999)
 '(scroll-margin 3)
 '(scroll-preserve-screen-position 1)
 '(sentence-end-double-space nil)
 '(server-host system-name)
 '(server-use-tcp t)
 '(smex-save-file "~/.emacs.d/.smex-items")
 '(tab-width 4)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/.undo"))))
 '(uniquify-ignore-buffers-re "^\\*")
 '(whitespace-style
   (quote
    (face faces space-before-tab tab-mark tabs trailing))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "snow"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "light slate blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "honeydew"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "royal blue"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red")))))
