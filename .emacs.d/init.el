;; Reduce GC during startup for faster loading
(defvar my/original-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; Set initial buffer before loading config
(setq initial-buffer-choice (lambda () (get-buffer "*Messages*")))

(load "~/.emacs.d/appearance.el")
(load "~/.emacs.d/general.el")
(load "~/.emacs.d/general-programming.el")
(load "~/.emacs.d/python-stuff.el")
(load "~/.emacs.d/elisp-stuff.el")
(load "~/.emacs.d/rb-client.el")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-selection ((t (:background "#b3ccf5" :foreground "black"))))
 '(tab-bar-tab ((t (:background "gray60" :foreground "black" :weight bold))))
 '(tab-bar-tab-inactive ((t (:background "gray85" :foreground "black")))))

(add-hook 'after-init-hook
          (lambda ()
            ;; Reset GC threshold to original value after startup
            (setq gc-cons-threshold my/original-gc-cons-threshold)
            (message "*** Emacs loaded in %s ***" (emacs-init-time))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(package-selected-packages
   '(helm-ag yasnippet vterm tuareg swift-mode origami opam-switch-mode ocp-indent ns-auto-titlebar magit lsp-ui lsp-sourcekit lsp-pyright lsp-java lsp-haskell key-seq highlight-symbol helm-xref helm-rg helm-projectile helm-lsp flycheck exec-path-from-shell evil-vimish-fold evil-search-highlight-persist evil-leader evil-commentary esup doom-themes dockerfile-mode diff-hl company benchmark-init auto-dim-other-buffers apheleia aidermacs adaptive-wrap)))
