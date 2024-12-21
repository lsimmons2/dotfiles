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


(use-package opam-switch-mode
  :ensure t
  :hook
  ((coq-mode tuareg-mode) . opam-switch-mode))

(add-hook 'tuareg-mode-hook #'lsp)
(add-hook 'caml-mode-hook #'lsp)

(use-package tuareg
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode))

;; Use ocp-indent for indentation
(use-package ocp-indent
  :ensure t
  :hook
  ;; Set up ocp-indent to run before save in Tuareg mode
  (tuareg-mode . (lambda ()
                   (add-hook 'before-save-hook 'ocp-indent-buffer nil 'local))))
