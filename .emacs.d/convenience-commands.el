

(defun reload-emacs-config ()
  "Reload Emacs configuration."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Emacs configuration reloaded!"))

(message "Loaded everything-else.el!!")
