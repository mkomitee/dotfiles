(require-package 'python)

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \[[0-9]+\]: "
 python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
 python-shell-completion-setup-code ""
 python-shell-completion-string-code "';'.join(get_ipython().complete('''%s''')[1])\n")

; Highlight the call to ipdb
; src http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
(defun annotate-pdb ()
  (interactive)
  (highlight-phrase "import ipdb")
  (highlight-phrase "ipdb.set_trace()")
  (highlight-phrase "import pdb")
  (highlight-phrase "pdb.set_trace()"))

(add-hook 'python-mode-hook 'annotate-pdb)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (setq fci-rule-column 80)))

(provide 'custom-python)
