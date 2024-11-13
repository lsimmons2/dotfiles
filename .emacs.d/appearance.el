
;;(load-theme 'doom-one t)
(load-theme 'doom-feather-light t)

(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)


(setq display-buffer-base-action '(display-buffer-same-window))


(defun my-dired-highlight-line ()
  "Highlight the entire line under the cursor in Dired mode."
  (hl-line-mode 1))  ;; Enable hl-line-mode for line highlighting
(add-hook 'dired-mode-hook 'my-dired-highlight-line)



(setq-default mode-line-buffer-identification
              '(:eval (if (and (featurep 'projectile) (projectile-project-root))
                          (file-relative-name buffer-file-name (projectile-project-root))
                        "%b")))

