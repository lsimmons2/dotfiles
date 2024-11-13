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
  :ensure t
  :config
  (global-company-mode))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :hook ((haskell-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-haskell
  :ensure t
  :after lsp-mode)

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

(use-package evil
  :ensure t
  :config
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
  :init
  (require 'helm)
  :config
  (helm-mode 1)
  ;; Bind TAB to expand without opening the action menu
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; Custom keybindings for navigating Helm lists
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  :bind (("M-x" . helm-M-x)
         ("C-x C-p" . project-find-file)
         ("C-x C-b" . helm-buffers-list)))

(with-eval-after-load 'helm-buffers
  (define-key helm-buffer-map (kbd "DEL") 'helm-buffer-run-kill-buffers))


(setq helm-boring-file-regexp-list
      '("\\.git$"
        "node_modules"
        "target"
        "dist"))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'helm)
  ;;:bind-keymap
  ;;("C-c p" . projectile-command-map)
  )

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on))

(defun helm-projectile-switch-project-dired ()
  "Switch to a recent project and open it in Dired."
  (interactive)
  (let ((projectile-switch-project-action #'projectile-dired))
    (helm-projectile-switch-project)))

(global-set-key (kbd "C-x C-d") 'helm-projectile-switch-project-dired)



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
