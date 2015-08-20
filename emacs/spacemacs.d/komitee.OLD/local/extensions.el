(setq komitee-post-extensions
      '(evil-quickscope
        ))

(defun komitee/init-evil-quickscope ()
    (progn
      (require 'evil-quickscope)
      (global-evil-quickscope-mode 1)
      )
  )
