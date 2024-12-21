
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp))

(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (setq tab-width 2) ;; Display tabs as 2 spaces
            (setq typescript-ts-mode-indent-offset 2))) ;; Set indent level to 2 spaces
