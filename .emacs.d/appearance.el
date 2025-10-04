;;TODO: remove this if I can remove the title bar alltogether?
;; title bar to change color depending on light/dark mode
(use-package ns-auto-titlebar
  :ensure t
  :config
  (ns-auto-titlebar-mode))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(defvar my/dark-theme 'doom-one)
(defvar my/light-theme 'doom-feather-light)

(defun my/detect-macos-dark-mode ()
  "Check if macOS is in dark mode and return t if it is."
  (let ((appearance (string-trim
                     (shell-command-to-string "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light"))))
    (string= appearance "Dark")))

(defun my/apply-theme (theme)
  "Apply THEME, disabling any other active themes to prevent conflicts."
  (mapc #'disable-theme custom-enabled-themes) ;; Disable current themes
  (load-theme theme t)
  (message "Switched to theme: %s" theme)
  )

(defun my/toggle-theme-based-on-system ()
  "Toggle between `my/dark-theme` and `my/light-theme` based on macOS appearance."
  (if (my/detect-macos-dark-mode)
      (progn
        (if (not (member my/dark-theme custom-enabled-themes))
            (my/apply-theme my/dark-theme)))
    (progn
      (if (not (member my/light-theme custom-enabled-themes))
          (my/apply-theme my/light-theme)))))


;; Run once on startup to set the correct theme
(my/toggle-theme-based-on-system)

;; Check and toggle theme every 3 seconds
(run-at-time nil (* 3 1) 'my/toggle-theme-based-on-system)


					;(load-theme 'doom-feather-light t)
					;(load-theme 'doom-one t)

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
              '(:eval (if (and (featurep 'projectile)
                               (projectile-project-root)
                               buffer-file-name)
                          (file-relative-name buffer-file-name (projectile-project-root))
                        "%b")))
					;(setq-default mode-line-buffer-identification
					;'(:eval (if (and (featurep 'projectile) (projectile-project-root))
					;(file-relative-name buffer-file-name (projectile-project-root))
					;"%b")))




(set-face-attribute 'default nil
                    :family "Monaco"   ;; Replace "Menlo" with your preferred font family
                    :height 140)      ;; Replace 140 with the desired font size (e.g., 140 for 14pt)

(defun switch-to-laptop-size ()
  "Switch font to a smaller size suitable for laptop screens."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :height 120)
  (message "Switched to laptop font size (120)"))

(defun switch-to-monitor-size ()
  "Switch font to a larger size suitable for external monitors."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :height 140)
  (message "Switched to monitor font size (140)"))

(defun my/customize-lsp-ui-sideline ()
  "Customize `lsp-ui-sideline` faces based on the current theme."
  (let ((bg (face-attribute 'default :background))   ;; Background from default face
        (fg (face-attribute 'shadow :foreground))   ;; Foreground from shadow face
        (info-fg (face-attribute 'success :foreground)) ;; Info from success face
        (warning-fg (face-attribute 'warning :foreground)) ;; Warning from warning face
        (error-fg (face-attribute 'error :foreground))) ;; Error from error face

    ;; Customize the base sideline face
    (set-face-attribute 'lsp-ui-sideline-global nil
                        :background bg
                        :foreground fg)

    ;; Customize the diagnostic face for errors
    (set-face-attribute 'lsp-ui-sideline-symbol nil
                        :background bg
                        :foreground fg)

    ;; Customize the current symbol face
    (set-face-attribute 'lsp-ui-sideline-current-symbol nil
                        :background bg
                        :foreground info-fg
                        :weight 'bold)

    ;; Customize code actions (like rename suggestions)
    (set-face-attribute 'lsp-ui-sideline-code-action nil
                        :background bg
                        :foreground info-fg
                        :weight 'bold)))

;; Apply the customizations when loading lsp-ui
;; (with-eval-after-load 'lsp-ui
;;   (my/customize-lsp-ui-sideline))



;; in the section of the modeline that normall shows the major and all the minor modes, just show the major mode
(setq minor-mode-alist nil)

(defun my/customize-mode-line ()
  "Customize mode-line to have distinct colors for active/inactive windows."
  (let ((active-bg (face-attribute 'company-tooltip-search :background nil t))
        (active-fg (face-attribute 'company-tooltip-search :foreground nil t))
        (inactive-bg (face-attribute 'mode-line-inactive :background))
        (default-bg (face-attribute 'default :background)))
    (set-face-attribute 'mode-line nil
                        :background active-bg
                        :foreground active-fg
                        :box nil)
    (set-face-attribute 'mode-line-inactive nil
                        :background inactive-bg
                        :box nil)
    ;; seems that header line inherits from company-tooltip-search/something upstream from it? or something like that?
    ;; adding this part so it doesn't change to the new active modeline colors
    (set-face-attribute 'header-line nil
			:inherit 'unspecified
			:box nil)
    ))

(add-hook 'after-init-hook 'my/customize-mode-line)
(advice-add 'load-theme :after (lambda (&rest _) (my/customize-mode-line)))

(custom-set-faces
 '(company-tooltip-selection ((t (:background "#b3ccf5" :foreground "black")))))
