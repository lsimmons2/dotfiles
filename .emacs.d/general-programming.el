
(add-hook 'prog-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

(use-package origami
  :ensure t
  :hook (prog-mode . origami-mode))


;; Function to conditionally enable LSP (skip for TRAMP connections)
(defun my/lsp-deferred-unless-tramp ()
  "Enable LSP unless we're in a TRAMP buffer."
  (lsp-deferred))
;; (unless (file-remote-p default-directory)
;;   (lsp-deferred)))

(use-package lsp-mode
  :ensure t
  :hook
  ((typescript-ts-mode . my/lsp-deferred-unless-tramp)
   (tsx-ts-mode . my/lsp-deferred-unless-tramp)
   (js-mode . my/lsp-deferred-unless-tramp)
   (js-jsx-mode . my/lsp-deferred-unless-tramp)
   (java-mode . my/lsp-deferred-unless-tramp)
   (python-mode . my/lsp-deferred-unless-tramp)
   (python-ts-mode . my/lsp-deferred-unless-tramp)  ;; Add this line
   (lsp-mode . lsp-diagnostics-mode))
  ;; Rest of your config remains the same
  :init
  (setq lsp-auto-guess-root t)
  ;; atow: putting this here so that lsp doesn't complain about no servers being available
  ;; when accessing a file via TRAMP
  (setq lsp-warn-no-matched-clients nil)
  :custom
  (lsp-file-watch-ignored
   '(
     '("[/\\\\]\\.git$"
       "[/\\\\]\\.github$"
       "[/\\\\]node_modules$"
       "[/\\\\]dist$"
       "[/\\\\]build$"
       "[/\\\\]vendor$"
       "/Users/leo" ;; TODO: don't know why I (maybe) need to add this here explicitly
       )))
  )


(use-package dap-mode
  :after lsp-mode
  :config
  (require 'dap-python)
  ;; (require 'dap-hydra)
  (setq dap-python-debugger 'debugpy)
  
  (evil-define-key 'normal dap-mode-map
    
    (kbd "SPC d d") 'dap-debug
    (kbd "SPC d e") 'dap-eval
    (kbd "SPC d c") 'dap-continue
    (kbd "SPC d n") 'dap-next
    (kbd "SPC d i") 'dap-step-in
    (kbd "SPC d o") 'dap-step-out
    (kbd "SPC d h") 'dap-hydra
    (kbd "SPC d b") 'dap-breakpoint-toggle
    (kbd "SPC d r") 'dap-debug-restart
    (kbd "SPC d l") 'dap-ui-locals
    (kbd "SPC d s") 'dap-ui-sessions
    (kbd "SPC d p") 'dap-ui-expressions
    (kbd "SPC d w") 'dap-ui-watch
    (kbd "SPC d q") 'dap-disconnect)
  
  ;; Enable dap-mode and its helper modes only for python for now
  (add-hook 'python-mode-hook 'dap-mode)
  (add-hook 'python-mode-hook 'dap-ui-mode)
  (add-hook 'python-mode-hook 'dap-tooltip-mode)

  (tooltip-mode 1) ;; NB keeping this always on per https://claude.ai/chat/720e6166-e7f2-4e2d-b8ac-f14e55293ed1 on 06.23.2025
  ;; (dap-ui-controls-mode 1)
  
  ;; Displaying debug windows on session startup
  ;; (add-hook 'dap-session-created-hook
  ;; (lambda (&_rest) (dap-hydra)))
  
  ;;; Display debug buffer after stepping
  ;; (add-hook 'dap-stopped-hook
  ;; (lambda (&_rest) (dap-hydra)))
  
  )



;; Clear Edebug's default keybindings to restore Evil functionality
(setq edebug-mode-map (make-sparse-keymap))
;; Add Evil integration hook
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

;; Bindings for editing Emacs Lisp files
(evil-define-key 'normal emacs-lisp-mode-map
  (kbd "SPC d d") 'edebug-defun                    ; instrument function
  )

;; Bindings for when you're actively debugging
(evil-define-key 'normal edebug-mode-map
  (kbd "SPC d e") 'edebug-eval-expression    ; evaluate expression
  (kbd "SPC d c") 'edebug-go-mode           ; continue
  (kbd "SPC d n") 'edebug-step-mode          ; next/step
  (kbd "SPC d i") 'edebug-step-in            ; step into
  (kbd "SPC d o") 'edebug-step-out           ; step out
  (kbd "SPC d b") 'edebug-set-breakpoint     ; toggle breakpoint
  (kbd "SPC d r") 'edebug-stop               ; stop/restart
  (kbd "SPC d l") 'edebug-bounce-point       ; show current location
  (kbd "SPC d s") 'edebug-where              ; show status
  (kbd "SPC d p") 'edebug-view-outside       ; view outside windows
  (kbd "SPC d w") 'edebug-where              ; where am I
  (kbd "SPC d q") 'edebug-top-level-nonstop  ; quit debugging
  )

;; Optional: Configure Edebug behavior
(setq edebug-all-defs nil)           ; Don't instrument everything by default
(setq edebug-all-forms nil)          ; Don't instrument all forms
(setq edebug-save-windows t)         ; Save window configuration


(add-hook 'apheleia-mode-hook
          (lambda ()
            (remove-hook 'before-save-hook 'lsp--before-save t)))


;;(with-eval-after-load 'lsp-mode
;;(setq lsp-disabled-clients '(pylsp pyls mspyls ruff-lsp semgrep-lsp)) ;; Disable other clients
;;(add-to-list 'lsp-enabled-clients 'pyright)) ;; Enable pyright

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  ;; defining this function to try to keep lsp-ui from wrapping and pushing lines of code down
  ;; per https://github.com/emacs-lsp/lsp-ui/issues/597
  ;; not sure it's actually working though - hard to reproduce 
  ;; (defun lsp-ui-sideline--compute-height nil '(height unspecified))
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  ;; (setq lsp-ui-sideline-show-diagnostics t)
  ;; (setq lsp-ui-sideline-show-hover t)
  ;; (setq lsp-ui-sideline-update-mode 'line)
  ;; (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-doc-enable nil) ;; Enable hover documentation
  )


					;TODO: is typescript-ts-mode-hook really from typescript-mode package?

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))


(use-package apheleia
  :ensure t
  :config
  (setq apheleia-log-only-errors nil)
  (setq apheleia-formatters-respect-indent-level nil)
  (setq apheleia-use-diff nil)

  ;; Use the built-in `run-prettier` formatter
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . prettier))

  (setf (alist-get 'ocamlformat apheleia-formatters)
        '("ocamlformat" "--name" buffer-file-name "-"))
  (add-to-list 'apheleia-mode-alist '(tuareg-mode . ocamlformat))
  (add-to-list 'apheleia-mode-alist '(caml-mode . ocamlformat))

  ;; Enable Apheleia globally
  (apheleia-global-mode +1))

(add-hook 'apheleia-post-format-hook
          (lambda ()
            (message "Apheleia successfully formatted the buffer!")))


;;TODO: not sure I need/want exec-path-from-shell package, installing it atow
;;to try to get apheleia to have access to globally-installed prettier
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; NB: atow 04.28.2025 flycheck is already installed somewhere, but can't find it so
;; adding the block here mainly to be able to put the python-mode hook in the use-package block
;; for flycheck
(use-package flycheck
  :ensure t
  :hook (python-mode . flycheck-mode)
  )


(defun copy-flycheck-error-at-point ()
  "Copy the flycheck error message at point to the kill ring."
  (interactive)
  (let ((errors (flycheck-overlay-errors-at (point))))
    (if (not errors)
        (message "No flycheck error at point")
      (let* ((error (car errors))
             (error-message (flycheck-error-message error))
             (error-id (flycheck-error-id error))
             (error-line (line-number-at-pos (flycheck-error-pos error)))
             (error-column (flycheck-error-column error))
             (error-level (flycheck-error-level error))
             (formatted-error 
              (format "%s:%d:%d: %s: %s%s"
                      (buffer-name)
                      error-line
                      (or error-column 0)
                      error-level
                      error-message
                      (if error-id (format " [%s]" error-id) ""))))
        (kill-new formatted-error)
        (message "Copied to clipboard: %s" formatted-error)))))

(with-eval-after-load 'evil
  (evil-define-key 'normal global-map (kbd "SPC y") 'copy-flycheck-error-at-point)
  )


(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "SPC o h") 'toggle-evil-search-highlight))

(defun my-toggle-comment ()
  "Toggle comments on the current line or region in normal, visual, or visual-block mode."
  (interactive)
  (if (use-region-p)  ;; If a region is selected
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;; Bind SPC c for commenting
(with-eval-after-load 'evil
  (evil-define-key 'normal global-map (kbd "SPC c") 'my-toggle-comment)
  (evil-define-key 'visual global-map (kbd "SPC c") 'my-toggle-comment))


(with-eval-after-load 'evil
  ;; (add-hook 'lsp-mode-hook
  (message "setting general lsp bindings!")
  (evil-define-key 'normal 'global
    (kbd "gd") 'lsp-find-definition        ;; Go to definition
    (kbd "gt") 'lsp-find-type-definition   ;; Go to type definition
    (kbd "gr") 'lsp-find-references        ;; Find references
    (kbd "SPC r") 'lsp-rename                 ;; Rename symbol
    (kbd "]e") 'flycheck-next-error        ;; Next Flycheck error
    (kbd "[e") 'flycheck-previous-error)   ;; Previous Flycheck error
  ;; )
  )



;;for going to references in list provided by lsp/helm
(defun my-xref-jump-to-location ()
  "Jump to the location under the cursor in the *xref* buffer."
  (interactive)
  (let ((xref-window (selected-window)))
    (xref-quit-and-goto-xref)
    (select-window xref-window)))
(with-eval-after-load 'evil
  (evil-define-key 'normal xref--xref-buffer-mode-map (kbd "RET") 'my-xref-jump-to-location))



(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  ;; Define Yasnippet keybindings with your leader key
  (evil-define-key 'normal global-map (kbd "SPC k i") 'yas-insert-snippet) ;; Insert snippet
  (evil-define-key 'normal global-map (kbd "SPC k n") 'yas-new-snippet)    ;; Create a new snippet
  (evil-define-key 'visual global-map (kbd "SPC k n") 'yas-new-snippet)    ;; Create a new snippet
  (evil-define-key 'normal global-map (kbd "SPC k a") 'yas-new-snippet)    ;; Create a new snippet
  (evil-define-key 'visual global-map (kbd "SPC k a") 'yas-new-snippet)    ;; Create a new snippet
  (evil-define-key 'normal global-map (kbd "SPC k f") 'yas-visit-snippet-file) ;; Visit/edit snippet
  (evil-define-key 'normal global-map (kbd "SPC k r") 'yas-reload-all)     ;; Reload snippets
  (evil-define-key 'normal global-map (kbd "SPC k d") 'yas-describe-tables) ;; Describe active snippets
  )

;; (use-package yasnippet-snippets
;;   :ensure t)


					;(use-package posframe
					;:ensure t
					;:config
					;(defun my/eldoc-posframe-display (info _context)
					;"Display Eldoc INFO in a posframe below the cursor.
                                        ;INFO may be a string or a list. _CONTEXT is ignored."
					;(if info
					;(let* ((clean-info (if (stringp info)
					;info
					;(mapconcat #'identity (flatten-list info) "\n"))))
          ;;; Wrap text to a maximum width of 80 characters while preserving properties
					;(setq clean-info (with-temp-buffer
					;(insert clean-info)
                             ;;; Fill region without stripping text properties
					;(let ((fill-column 80))
					;(fill-region (point-min) (point-max)))
					;(buffer-substring (point-min) (point-max))))
          ;;; Display in posframe
					;(posframe-show " *eldoc-posframe*"
					;:string clean-info
					;:poshandler 'posframe-poshandler-point-bottom-left-corner
					;:background-color (face-attribute 'tooltip :background)
					;:foreground-color (face-attribute 'tooltip :foreground)
					;:width 80
					;:height 10
					;:hidehandler #'posframe-hidehandler-quick-setup))
      ;;; Hide posframe if there is no INFO
					;(posframe-hide " *eldoc-posframe*")))

  ;;; Hide the posframe when switching buffers
					;(add-hook 'buffer-list-update-hook
					;(lambda () (posframe-hide " *eldoc-posframe*")))

  ;;; Hide the posframe when navigating to a different part of the buffer
					;(add-hook 'post-command-hook
					;(lambda ()
					;(unless (eldoc--documentation-function) ;; Check if eldoc info is available
					;(posframe-hide " *eldoc-posframe*"))))

  ;;; Set `my/eldoc-posframe-display` as the display function for eldoc
					;(setq eldoc-display-functions '(my/eldoc-posframe-display)))



(defun my-debugger-setup (buffer alist)
  "Custom display function for the debugger."
  (let ((window (display-buffer-in-side-window buffer alist)))
    (with-selected-window window
      ;; Resize the window to 30% of the frame height
      (window-resize window (- (floor (* 0.2 (frame-height))) (window-total-height))))
    window))

(setq display-buffer-alist
      '(("\\*Backtrace\\*"
         (my-debugger-setup))))

(defun rz-dired ()
  (interactive)
  (find-file (concat "/ssh:rz:" "/home/leo/dev/research-buddy"))
  )


(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol
  :config
  (evil-define-key 'normal global-map (kbd "C-m") 'helm-lsp-workspace-symbol)
  )


(defun my/show-lsp-symbols-with-colored-types ()
  "Display all LSP symbols in current buffer with color-coded types."
  (interactive)
  (let* ((text-document (lsp--text-document-identifier))
         (params (lsp-make-document-symbol-params :text-document text-document))
         (symbols (lsp-request "textDocument/documentSymbol" params))
         (candidates (mapcar (lambda (sym)
                               (let* ((name (gethash "name" sym))
                                      (kind (gethash "kind" sym))
                                      (kind-str (alist-get kind lsp-symbol-kinds "Unknown"))
                                      ;; Choose face based on symbol kind
                                      (face (cond
                                             ((= kind 5) 'font-lock-type-face)          ;; Class
                                             ((= kind 6) 'font-lock-function-name-face)  ;; Method
                                             ((= kind 12) 'font-lock-function-name-face) ;; Function
                                             ((= kind 13) 'font-lock-variable-name-face) ;; Variable
                                             ((= kind 14) 'font-lock-constant-face)      ;; Constant
                                             (t 'font-lock-keyword-face))))              ;; Others
                                 ;; Format with properties
                                 (cons (format "%s: %s" 
                                               (propertize kind-str 'face face)
                                               name)
                                       sym)))
                             symbols)))
    
    ;; Now use the candidates list
    (helm :sources
          (helm-build-sync-source "LSP Symbols"
            :candidates candidates
            :fuzzy-match t
            :filtered-candidate-transformer
            (lambda (candidates _source)
              ;; This transformer ensures what we're matching against
              (mapcar (lambda (candidate)
                        ;; Create a new cons with same car but preserve cdr
                        (cons (car candidate) (cdr candidate)))
                      candidates))
            :action (lambda (candidate)
                      (let* ((range (gethash "range" candidate))
                             (start (gethash "start" range)))
                        (goto-char (lsp--position-to-point start)))))
          :buffer "*helm lsp symbols*")))

(evil-define-key 'normal 'global (kbd "C-m") 'my/show-lsp-symbols-with-colored-types)
