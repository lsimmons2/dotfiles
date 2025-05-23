(add-to-list 'load-path "/Users/leo/.opam/default/share/emacs/site-lisp")

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

;; (add-hook 'tuareg-mode-hook #'lsp)
;; (add-hook 'caml-mode-hook #'lsp)

(use-package tuareg
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode))


(use-package ocp-indent
  :ensure t
  :hook
  (tuareg-mode . (lambda ()
                   (add-hook 'before-save-hook
                             (lambda ()
                               (save-restriction
                                 ;; Ensure entire buffer is accessible
                                 (widen)
                                 ;; Indent the accessible region
                                 (ocp-indent-region (point-min) (point-max))))
                             nil
                             'local))))


(with-eval-after-load 'evil
  ;; OCaml-specific overrides using Merlin
  (add-hook 'tuareg-mode-hook
            (lambda ()
	      (message "Setting ocaml merlin bindings!!!")
              (evil-local-set-key 'normal (kbd "gd") 'merlin-locate)
              (evil-local-set-key 'normal (kbd "gt") 'merlin-type-enclosing)
              (evil-local-set-key 'normal (kbd "gr") 'merlin-project-occurrences)
	      (evil-local-set-key 'normal (kbd "]e") 'merlin-error-next)
	      (evil-local-set-key 'normal (kbd "[e") 'merlin-error-prev)
	      (evil-normalize-keymaps)
	      )))


(defun merlin-type-enclosing-popup ()
  "Show `merlin-type-enclosing` result in a popup window instead of the minibuffer."
  (interactive)
  (let ((type-output (merlin-type-enclosing)))
    (if type-output
        (with-output-to-temp-buffer "*Merlin Type*"
          (princ (mapconcat #'identity type-output "\n")))
      (message "No type information available."))))

(defun merlin-type-enclosing-posframe ()
  "Show `merlin-type-enclosing` result in a floating popup using `posframe`."
  (interactive)
  (let ((type-output (merlin-type-enclosing)))
    (if type-output
        (posframe-show "*Merlin Type*"
                       :string (mapconcat #'identity type-output "\n")
                       :position (point))
      (message "No type information available."))))

(defun merlin-type-enclosing-popup-new ()
  "Run `merlin-type-enclosing` and force `*merlin-types*` to show in a popup."
  (interactive)
  (merlin-type-enclosing)
  (when-let ((buf (get-buffer "*merlin-types*")))
    (pop-to-buffer buf)))


(defun merlin-type-enclosing-posframe-new ()
  "Show `merlin-type-enclosing` result in a floating posframe."
  (interactive)
  (merlin-type-enclosing)
  (when-let ((buf (get-buffer "*merlin-types*")))
    (posframe-show "*merlin-types*" :position (point))))


;; (require 'dap-mode)
;; (require 'dap-codelldb)
;; (require 'dap-ocaml)

;; (use-package dap-mode
;;   :ensure t)



;; (use-package dap-codelldb
;;   :ensure t)

;; (use-package dap-ocaml
;;   :ensure t)


;; TODO: there might be a more elegant way to do this, for now just hard-coding global switch
;; binaries into exec-path and PATH emacs env var (used by emacs-spawned processes?) so that it
;; can find ocamlearlybird, and other such binaries
;; (add-to-list 'exec-path "/Users/leo/.opam/default/bin")
;; (setenv "PATH" (concat "/Users/leo/.opam/default/bin:" (getenv "PATH")))
;; (add-to-list 'exec-path "/Users/leo/dev/ocaml-playground/debugging_test/_opam/bin")
;; (setenv "PATH" (concat "/Users/leo/dev/ocaml-playground/debugging_test/_opam/bin:" (getenv "PATH")))


;; (setq dap-print-io t)  ;; Log DAP messages to *Messages*
;; (setq dap-auto-configure-features '(sessions locals breakpoints expressions))
;; (setq dap-output-buffer-filter '("stdout" "stderr" "debug output")) ;; Show more output
;; (setq dap-server-log t)  ;; Force creation of *dap-server* buffer



;; (dap-register-debug-provider
;;  "ocaml.earlybird-verbose"
;;  (lambda (conf)
;;    (plist-put conf :dap-server-path '("env" "FOO=BAR" "ocamlearlybird" "debug" "--verbosity=debug"))
;;    conf))

;; (dap-register-debug-provider
;;  "ocaml.earlybird-simple"
;;  (lambda (conf)
;;    (plist-put conf :dap-server-path '("ocamlearlybird" "debug" "--verbosity=debug"))
;;    conf))


;; (dap-register-debug-provider
;;  "ocaml.earlybird-with-logs"
;;  (lambda (conf)
;;    (plist-put conf :dap-server-path '("ocamlearlybird" "debug" "--verbosity=debug"))
;;    conf))

;; (dap-register-debug-template "OCaml Earlybird verbose"
;; 			     (list :type "ocaml.earlybird-verbose"
;; 				   :request "launch"
;; 				   :name "Run OCaml Debugger"
;; 				   :program "/Users/leo/dev/ocaml-playground/debugging_test/_build/default/fib.bc"
;; 				   :arguments nil))

;; (dap-register-debug-template "OCaml Earlybird simple"
;; 			     (list :type "ocaml.earlybird-simple"
;; 				   :request "launch"
;; 				   :name "Run OCaml Debugger"
;; 				   :program "/Users/leo/dev/ocaml-playground/debugging_test/_build/default/fib.bc"
;; 				   :arguments nil))


;; LAST ONE NOT COMMENTED OUT
;; (dap-register-debug-template "Run OCaml Debugger (experimental)"
;; 			     (list :type "ocaml.earlybird"
;; 				   :request "launch"
;; 				   :name "Run OCaml Debugger (experimental)"
;; 				   :program "/Users/leo/dev/ocaml-playground/debugging_test/_build/default/fib.bc"
;; 				   :stopOnEntry t
;; 				   :cwd "/Users/leo/dev/ocaml-playground/debugging_test/"))



;; :stopOnEntry t
;; :program "/Users/leo/dev/ocaml-playground/debugging_test/_opam/bin/ocamlearlybird"

;; (defun dap--send-message (message callback debug-session)
;;   "MESSAGE DEBUG-SESSION CALLBACK."
;;   (if (dap--session-running debug-session)
;;       (let* ((request-id (cl-incf (dap--debug-session-last-id debug-session)))
;;              (message (plist-put message :seq request-id)))
;;         (puthash request-id callback (dap--debug-session-response-handlers debug-session))
;;         (when dap-print-io
;;           (let ((inhibit-message dap-inhibit-io))
;;             (message "Sending: \n%s" (dap--json-encode message))))

;;         (process-send-string (dap--debug-session-proc debug-session)
;;                              (dap--make-message message)))
;;     (error "Session %s is already terminated" (dap--debug-session-name debug-session))))
