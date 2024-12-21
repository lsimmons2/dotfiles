(use-package lsp-haskell
  :ensure t
  :after lsp-mode
  :config
  (setenv "PATH" (concat (getenv "PATH") ":/Users/leo/.ghcup/bin"))
  (setq exec-path (append exec-path '("/Users/leo/.ghcup/bin"))))


(use-package haskell-mode
  :ensure t)
