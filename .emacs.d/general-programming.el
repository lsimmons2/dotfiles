
;; Treat underscore as part of a word so evil motions (w, b, e) don't split on it
;; e.g. `w` on foo_bar moves to the next word, not to _bar
(add-hook 'prog-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

(use-package origami
  :ensure t
  :hook (prog-mode . origami-mode))

(setq lsp-log-io nil)
;; Function to conditionally enable LSP (only in projectile projects)
(defun my/lsp-deferred-conditional ()
  "Enable LSP only if we're in a projectile project. Works for both local and TRAMP buffers."
  (let ((has-projectile (fboundp 'projectile-project-root))
        (project-root (condition-case nil
                          (when (fboundp 'projectile-project-root)
                            (projectile-project-root))
                        (error nil))))
    (cond
     ((not has-projectile)
      (message "my/lsp-deferred-conditional: Projectile not available in buffer, skipping LSP: %s" (buffer-file-name)))
     ((not project-root)
      (message "my/lsp-deferred-conditional: Not in a projectile project, skipping LSP for: %s" (buffer-file-name)))
     (t
      (message "my/lsp-deferred-conditional: Starting in project %s for file: %s" project-root (buffer-file-name))
      (lsp-deferred)))))

(use-package lsp-mode
  :ensure t
  :hook
  ((typescript-ts-mode . my/lsp-deferred-conditional)
   (tsx-ts-mode . my/lsp-deferred-conditional)
   (js-mode . my/lsp-deferred-conditional)
   (js-jsx-mode . my/lsp-deferred-conditional)
   (python-mode . my/lsp-deferred-conditional)
   (python-ts-mode . my/lsp-deferred-conditional)  ;; Add this line
   (lsp-mode . lsp-diagnostics-mode))
  ;; Rest of your config remains the same
  :init
  ;; atow: putting this here so that lsp doesn't complain about no servers being available
  ;; when accessing a file via TRAMP
  ;; (setq lsp-warn-no-matched-clients nil)
  ;; Kill workspace when closing last buffer to prevent accumulation
  (setq lsp-keep-workspace-alive nil)
  ;; Prevent lsp-mode from auto-configuring dap-mode
  (setq lsp-enable-dap-auto-configure nil)
  ;; Disable flycheck - lsp-mode should not enable it
  (setq lsp-diagnostics-provider :none))


(use-package dap-mode
  :after lsp-mode
  :config
  ;; Disable auto-configure mode which enables dap-mode globally
  (dap-auto-configure-mode -1)

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
  
  )



(add-hook 'apheleia-mode-hook
          (lambda ()
            (remove-hook 'before-save-hook 'lsp--before-save t)))


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
  ;; :config
  ;; (run-with-idle-timer 1 nil #'os/setup-install-grammars)
  )


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
  :defer 2
  :config
  (exec-path-from-shell-initialize))

;; NB: atow 04.28.2025 flycheck is already installed somewhere, but can't find it so
;; adding the block here mainly to be able to put the python-mode hook in the use-package block
;; for flycheck
;; then turning it off again on 10.18.2025 since I'm not really using it
;; (use-package flycheck
;;   :ensure t
;;   :hook (python-mode . flycheck-mode)
;;   )


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


(defun my/goto-definition ()
  "Go to definition using LSP."
  (interactive)
  (lsp-find-definition))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "gd") 'my/goto-definition        ;; Go to definition
    (kbd "C-]") 'my/goto-definition       ;; Go to definition (vim-standard jump-to-tag)
    (kbd "gt") 'lsp-find-type-definition   ;; Go to type definition
    (kbd "gr") 'lsp-find-references        ;; Find references
    (kbd "SPC r") 'lsp-rename                 ;; Rename symbol
    (kbd "]e") 'flycheck-next-error        ;; Next Flycheck error
    (kbd "[e") 'flycheck-previous-error)   ;; Previous Flycheck error
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


(use-package dockerfile-mode
  :ensure t)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)


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

(defun my/show-symbols ()
  "Show symbols in current buffer - use Elisp symbols for elisp files, LSP for others."
  (interactive)
  (cond
   ;; For Emacs Lisp files, use helm-imenu
   ((derived-mode-p 'emacs-lisp-mode)
    (helm-imenu))
   ;; For LSP-enabled buffers, use LSP symbols
   ((bound-and-true-p lsp-mode)
    (my/show-lsp-symbols-with-colored-types))
   ;; Fallback to imenu for other modes
   (t
    (helm-imenu))))

(evil-define-key 'normal 'global (kbd "C-m") 'my/show-symbols)

(defun my/search-project-symbols ()
  "Search for symbols project-wide - use helm-imenu-in-all-buffers for elisp files, LSP workspace symbols for others."
  (interactive)
  (cond
   ;; For Emacs Lisp files, use helm-imenu across all buffers
   ((derived-mode-p 'emacs-lisp-mode)
    (helm-imenu-in-all-buffers))
   ;; For LSP-enabled buffers, use helm-lsp-workspace-symbol
   ((bound-and-true-p lsp-mode)
    (call-interactively 'helm-lsp-workspace-symbol))
   ;; Fallback to helm-imenu-in-all-buffers for other modes
   (t
    (helm-imenu-in-all-buffers))))

(evil-define-key 'normal 'global (kbd "SPC m") 'my/search-project-symbols)
