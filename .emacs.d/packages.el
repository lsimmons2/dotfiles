(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install and configure packages using `use-package`
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package ns-auto-titlebar
  :ensure t
  :config
  (ns-auto-titlebar-mode))

(use-package company
  :disabled t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package lsp-mode
  :ensure t
  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js-jsx-mode . lsp-deferred)
         (lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-completion-provider :capf)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-log-io nil)
  )

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

(use-package lsp-haskell
  :ensure t
  :after lsp-mode
  :config
  (setenv "PATH" (concat (getenv "PATH") ":/Users/leo/.ghcup/bin"))
  (setq exec-path (append exec-path '("/Users/leo/.ghcup/bin"))))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2))

(use-package evil
  :ensure t
  :config
  (setq evil-undo-system 'undo-redo) ;; Use the default Emacs undo system with redo support
  (evil-mode 1))


(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode))

(use-package haskell-mode
  :ensure t)

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-boring-file-regexp-list
        '("\\.git$"
          "node_modules"
          "target"
          "dist")))

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)              ;; Enable caching for faster performance
  (setq projectile-globally-ignored-directories '(".git" "node_modules" "dist"))
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'helm)
  )

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp))

;TODO: is typescript-ts-mode-hook really from typescript-mode package?
(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (setq tab-width 2) ;; Display tabs as 2 spaces
            (setq typescript-ts-mode-indent-offset 2))) ;; Set indent level to 2 spaces


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


;(use-package lsp-eslint
  ;:demand t
  ;:after lsp-mode)

  ;(setf (alist-get 'prettier-json apheleia-formatters)
        ;'("prettier" "--stdin-filepath" filepath))




(use-package apheleia
  :ensure t
  :config
  (setq apheleia-log-only-errors nil)
  (setq apheleia-formatters-respect-indent-level nil)
  (setf (alist-get 'prettier apheleia-formatters)
        '("npx" "prettier" "--stdin-filepath" filepath))
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


;; this config given by ocaml setup - let's not change this
(add-to-list 'load-path "/Users/leo/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  ;; Register Merlin
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  ;; Automatically start it in OCaml buffers
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)
  ;; Use opam switch to lookup ocamlmerlin binary
  (setq merlin-command 'opam)
  ;; To easily change opam switches within a given Emacs session, you can
  ;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
  ;; and use one of its "OPSW" menus.
  ))


;;TODO: remove this if I can remove the title bar alltogether?
;; title bar to change color depending on light/dark mode
(use-package ns-auto-titlebar
  :if (eq system-type 'darwin)  ;; Only load on macOS
  :ensure t
  :config
  (ns-auto-titlebar-mode))  ;; Enable the auto titlebar adjustment

(use-package evil-search-highlight-persist
  :ensure t
  :config
  (global-evil-search-highlight-persist t)) ;; Enable persistent highlights globally
