(require-package 'python)
(require-package 'jedi)

(setq
 jedi:complete-on-dot t
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

(defun my-python-hook ()
  (progn
    (annotate-pdb)
    (fci-mode)
    (setq fci-rule-column 80)
    (jedi:setup)
    (add-to-list 'write-file-functions
                 'delete-trailing-whitespace)))
(add-hook 'python-mode-hook 'my-python-hook)

(evil-define-key 'insert python-mode-map (kbd "RET") 'evil-ret-and-indent)

(provide 'custom-python)
