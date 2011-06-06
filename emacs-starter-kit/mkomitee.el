;;; For emacsclient
(server-start)

(defconst emacs-config-dir "~/.emacs.d/configs/")
(defconst emacs-vendor-dir "~/.emacs.d/vendor/")
(defun get-subdirs (directory)
  "Get a list of subdirectories under a given directory"
  (apply 'nconc (mapcar (lambda (fa)
                        (and
                         (eq (cadr fa) t)
                         (not (equal (car fa) "."))
                         (not (equal (car fa) ".."))
                         (list (car fa))))
                        (directory-files-and-attributes directory))))

(defun add-dirs-to-loadpath (dir-name)
  "add subdirs of your vendor directory to the load path"
  (dolist (subdir (get-subdirs dir-name))
    (setq load-path (cons (concat dir-name subdir) load-path))
    (message "Added %s to load path" subdir)))

(add-dirs-to-loadpath emacs-vendor-dir)

(defun load-cfg-files (filelist)
  (dolist (file filelist)
    (let ((filename (expand-file-name (concat emacs-config-dir file ".el"))))
      (if (file-exists-p filename)
          (progn
            (load (concat filename))
            (message "Loaded config file: %s" filename))
       (message "Could not load file: %s" filename)))))

(load-cfg-files '("cfg_generic"
                  "cfg_textmate"
                  "cfg_python"
                  "cfg_perl"
                  "cfg_ruby"
                  "cfg_snippets"
                  "cfg_autocomplete"))


