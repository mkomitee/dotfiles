(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((c-mode . "bsd")
     (c++-mode . "bsd")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(custom-safe-themes
   (quote ("6cb86285d0cee020bcb0e9d0ffa56bad4c5be86d2bd14f2f4997a46130a6fe56"
           "7e8d09164066c3fc1f27a46cb9797e890648a52077fcfe07d91f041d9864a56d"
           "2a12e95e9ee6ed57592e7df12f3f028205575e9b3affdb5e6fa589421c618136"
           "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e"
           "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832"
           default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(evil-search-highlight-persist-highlight-face
   ((t (:inherit lazy-highlight))))
 '(evil-visual-mark-face
   ((t (:background "color-89" :foreground "yellow" :underline t))))
 '(flyspell-incorrect
   ((t (:background "#303030" :foreground "#ff4ea3" :underline
                    (:color "yellow1" :style wave) :weight normal))))
 '(helm-selection ((t (:foreground "forest green" :underline t))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "snow"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "light slate blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "honeydew"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "royal blue"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
 '(which-func ((t (:foreground "white")))))
