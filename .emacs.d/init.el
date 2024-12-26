(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(load "~/.emacs.d/startup.el")
(load "~/.emacs.d/appearance.el")
(load "~/.emacs.d/general.el")
(load "~/.emacs.d/general-programming.el")
(load "~/.emacs.d/ocaml-stuff.el")
(load "~/.emacs.d/haskell-stuff.el")
(load "~/.emacs.d/java-stuff.el")
(load "~/.emacs.d/txt-file-stuff.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(package-selected-packages
   '(yasnippet-snippets yasnippet evil-commentary tuareg helm-rg apheleia evil-leader key-chord lsp-haskell lsp-ui flycheck company ns-auto-titlebar doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-bar-tab ((t (:background "gray60" :foreground "black" :weight bold))))
 '(tab-bar-tab-inactive ((t (:background "gray85" :foreground "black")))))

(message "init.el finished")
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
