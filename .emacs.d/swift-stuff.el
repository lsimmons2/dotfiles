;; Swift mode for basic syntax highlighting and editing
(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'"
  :hook (swift-mode . lsp))

;; LSP client for SourceKit-LSP
(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable 
        (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))
