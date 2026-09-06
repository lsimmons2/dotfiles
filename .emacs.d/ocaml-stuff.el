;;; ocaml-stuff.el --- OCaml dev setup using merlin (no LSP)

;; This config assumes the active opam switch's bin dir is on PATH
;; (set by ~/.zshrc via opam-init/init.zsh) and that
;; exec-path-from-shell has propagated it into Emacs's exec-path.
;; So `ocamlmerlin', `dune', `ocamlformat', `ocp-indent', `utop' all
;; resolve to the binaries in the current opam switch.

(use-package tuareg
  :ensure t
  :mode (("\\.ml[ily]?\\'" . tuareg-mode)))

(use-package dune
  :ensure t)

(use-package merlin
  :ensure t
  :hook ((tuareg-mode . merlin-mode)
         (caml-mode   . merlin-mode))
  :config
  (setq merlin-error-after-save t)
  ;; Jump to definitions (C-], gd) in the current window instead of
  ;; splitting/using another window.
  (setq merlin-locate-in-new-window 'never)
  ;; Show the occurrences list (gr) in the current window too.
  (setq merlin-occurrences-show-buffer 'same))

(use-package merlin-eldoc
  :ensure t
  :hook (tuareg-mode . merlin-eldoc-setup))

(use-package merlin-company
  :ensure t
  :after (merlin company))

(use-package ocp-indent
  :ensure t
  :hook (tuareg-mode . ocp-setup-indent))

(use-package utop
  :ensure t
  :hook (tuareg-mode . utop-minor-mode)
  :config
  (setq utop-command "opam exec -- dune utop . -- -emacs"))

(with-eval-after-load 'evil
  (with-eval-after-load 'merlin
    (evil-define-key 'normal merlin-mode-map
      (kbd "gd")    'merlin-locate
      (kbd "C-]")   'merlin-locate
      (kbd "gt")    'merlin-locate-type
      (kbd "gr")    'merlin-occurrences
      (kbd "K")     'merlin-type-enclosing
      (kbd "SPC r") 'merlin-iedit-occurrences
      (kbd "]e")    'merlin-error-next
      (kbd "[e")    'merlin-error-prev)))

(provide 'ocaml-stuff)
