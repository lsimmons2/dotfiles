;;; appearance.el --- Visual appearance configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for themes, fonts, mode-line, and UI elements

;;; Code:

;;; Theme Configuration
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; Theme variables
(defvar my/dark-theme 'doom-one)
(defvar my/light-theme 'doom-feather-light)

;; Theme switching functions
(defun my/detect-macos-dark-mode ()
  "Check if macOS is in dark mode and return t if it is."
  (let ((appearance (string-trim
                     (shell-command-to-string "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light"))))
    (string= appearance "Dark")))

(defun my/apply-theme (theme)
  "Apply THEME, disabling any other active themes to prevent conflicts."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (message "Switched to theme: %s" theme))

(defun my/toggle-theme-based-on-system ()
  "Toggle between `my/dark-theme` and `my/light-theme` based on macOS appearance."
  (if (my/detect-macos-dark-mode)
      (when (not (member my/dark-theme custom-enabled-themes))
        (my/apply-theme my/dark-theme))
    (when (not (member my/light-theme custom-enabled-themes))
      (my/apply-theme my/light-theme))))

;;; UI Elements
;; Disable unnecessary UI elements
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Hide macOS titlebar
(add-to-list 'default-frame-alist '(undecorated . t))

;; Window behavior
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq display-buffer-base-action '(display-buffer-same-window))

;; Highlight current line
(global-hl-line-mode 1)

;;; Font Configuration
(set-face-attribute 'default nil
                    :family "Monaco"
                    :height 140)

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

;;; Mode-Line Configuration
;; Hide all modes (both major and minor)
(setq mode-line-modes nil)

;; Hide encoding information
(setq-default mode-line-mule-info nil)

;; Show line number in format "Line 123 / 456"
(setq-default mode-line-position
              '("Line %l / " (:eval (number-to-string (line-number-at-pos (point-max))))))

;; Show project-relative path in buffer identification
(setq-default mode-line-buffer-identification
              '(:eval (if (and (featurep 'projectile)
                               (projectile-project-root)
                               buffer-file-name)
                          (file-relative-name buffer-file-name (projectile-project-root))
                        "%b")))

;; Add default-directory to the end of mode-line (with ~ for home)
(setq-default mode-line-format
              (append mode-line-format '(" " (:eval (abbreviate-file-name default-directory)))))

;; Customize mode-line colors
(defun my/customize-mode-line ()
  "Customize mode-line to have distinct colors for active/inactive windows."
  (let ((active-bg (face-background 'completions-highlight nil t))
        (active-fg (face-foreground 'completions-highlight nil t))
        (inactive-bg (face-background 'mode-line-inactive nil t)))
    (set-face-attribute 'mode-line nil
                        :background active-bg
                        :foreground active-fg
                        :box nil)
    (set-face-attribute 'mode-line-inactive nil
                        :background inactive-bg
                        :box nil)
    (set-face-attribute 'header-line nil
                        :inherit 'mode-line-inactive
                        :box nil)))

;; Apply mode-line customization after theme loads
(advice-add 'load-theme :after (lambda (&rest _) (my/customize-mode-line)))

;; IMPORTANT: Must run after the advice above
;; Apply system theme on startup and check every 3 seconds
(my/toggle-theme-based-on-system)
(run-at-time nil (* 3 1) 'my/toggle-theme-based-on-system)

;;; Face Customization
(custom-set-faces
 '(company-tooltip-selection ((t (:background "#b3ccf5" :foreground "black")))))

(provide 'appearance)
;;; appearance.el ends here
