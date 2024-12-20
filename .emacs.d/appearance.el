
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

;(defun my/toggle-theme-based-on-system ()
  ;"Toggle between `my/dark-theme` and `my/light-theme` based on macOS appearance."
  ;(message "[DEBUG] Starting theme toggle based on system appearance...")
  ;(if (my/detect-macos-dark-mode)
      ;(progn
        ;(message "[DEBUG] macOS is in dark mode.")
        ;(if (member my/dark-theme custom-enabled-themes)
            ;(message "[DEBUG] Dark theme `%s` is already active. No changes needed." my/dark-theme)
          ;(message "[DEBUG] Switching to dark theme `%s`." my/dark-theme)
          ;(my/apply-theme my/dark-theme)))
    ;(progn
      ;(message "[DEBUG] macOS is in light mode.")
      ;(if (member my/light-theme custom-enabled-themes)
          ;(message "[DEBUG] Light theme `%s` is already active. No changes needed." my/light-theme)
        ;(message "[DEBUG] Switching to light theme `%s`." my/light-theme)
        ;(my/apply-theme my/light-theme))))
  ;(message "[DEBUG] Theme toggle complete.")
  ;)

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
