
(use-package lsp-mode
  :ensure t
  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js-jsx-mode . lsp-deferred)
	 (python-mode . lsp-deferred)
         (tuareg-mode . lsp-deferred)
         (lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config

  (setq lsp-enable-on-type-formatting nil) ;; Disable formatting triggered by typing
  (setq lsp-before-save-edits nil)         ;; Disable LSP formatting on save
  (setq lsp-completion-provider :capf)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-log-io nil)
  )

(add-hook 'apheleia-mode-hook
          (lambda ()
            (remove-hook 'before-save-hook 'lsp--before-save t)))


					;(with-eval-after-load 'lsp-mode
					;(setq lsp-disabled-clients '(pylsp pyls mspyls ruff-lsp semgrep-lsp)) ;; Disable other clients
					;(add-to-list 'lsp-enabled-clients 'pyright)) ;; Enable pyright

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-update-mode 'line)
  (setq lsp-ui-doc-enable t) ;; Enable hover documentation
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


					;TODO: not sure I need/want exec-path-from-shell package, installing it atow
					;to try to get apheleia to have access to globally-installed prettier
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))




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
  ;; General LSP mappings
  (key-chord-define evil-normal-state-map "gd" 'lsp-find-definition)
  (key-chord-define evil-normal-state-map "gt" 'lsp-find-type-definition)
  (key-chord-define evil-normal-state-map "gr" 'lsp-find-references)
  (key-chord-define evil-normal-state-map " r" 'lsp-rename)
  (key-chord-define evil-normal-state-map "]e" 'flycheck-next-error)
  (key-chord-define evil-normal-state-map "[e" 'flycheck-previous-error)

  ;; OCaml-specific overrides using Merlin
  (add-hook 'tuareg-mode-hook
            (lambda ()
              ;; Use Merlin instead of LSP for definition and type navigation
              (key-chord-define evil-normal-state-local-map "gd" 'merlin-locate)
              (key-chord-define evil-normal-state-local-map "gt" 'merlin-type-enclosing)
              (key-chord-define evil-normal-state-local-map "gr" 'merlin-project-occurrences))))



;;for going to references in list provided by lsp/helm
(defun my-xref-jump-to-location ()
  "Jump to the location under the cursor in the *xref* buffer."
  (interactive)
  (let ((xref-window (selected-window)))
    (xref-quit-and-goto-xref)
    (select-window xref-window)))
(with-eval-after-load 'evil
  (evil-define-key 'normal xref--xref-buffer-mode-map (kbd "RET") 'my-xref-jump-to-location))
