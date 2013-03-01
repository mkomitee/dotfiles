
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-to-list 'completion-ignored-extensions ".hi")
(setq haskell-program-name "ghci")

(provide 'custom-haskell)
