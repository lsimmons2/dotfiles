;; (use-package lsp-pyright
;;   :ensure t
;;   :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))  ; or lsp-deferred


(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") 
  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))
         (python-ts-mode . (lambda ()
                             (require 'lsp-pyright)
                             (lsp))))
  :custom
  (lsp-pyright-multi-root t)  ;; Enable multi-root for better project detection
  )

(dap-register-debug-template
 "Python: Run Script (Interactive)"
 (list :type "python"
       :args nil
       :cwd nil  ;; Will use the directory of the current buffer
       :env '(("PYTHONPATH" . ".")
	      ("REDIS_HOST" . "redis-server")
	      ("REDIS_PORT" . "6379"))
       :program nil  ;; This will prompt you each time
       :module nil
       :request "launch"
       :name "Python: Run Script (Interactive)"))

(dap-register-debug-template
 "Python: Wikipediaa"
 (list :type "python"
       :args nil
       :cwd default-directory  ;; Uses the current buffer's directory
       :env '(("PYTHONPATH" . ".")
	      ("REDIS_HOST" . "redis-server")
	      ("REDIS_PORT" . "6379"))
       :program (concat default-directory "wikipedia.py")
       :module nil
       :request "launch"
       :name "Python: Wikipedia Lemmatizer"))

(dap-register-debug-template
 "Python: Wikipedia"
 (list :type "python"
       :args nil
       :cwd default-directory  ;; Uses the current buffer's directory
       :env '(("PYTHONPATH" . ".")
	      ("REDIS_HOST" . "redis-server")
	      ("REDIS_PORT" . "6379"))
       :program (concat default-directory "wikipedia.py")
       :module nil
       :request "launch"
       :name "Python: Wikipedia Lemmatizer"))


(dap-register-debug-template
 "RB Server :: Test"
 (list :type "python"
       :cwd "/Users/leo/dev/research-buddy/"
       ;; :module "pytest"
       :request "launch"
       :env '(("PYTHONPATH" . "/Users/leo/dev/research-buddy")
              ("API_ENV" . "test"))
       :debugger 'debugpy
       :name "RB Server :: Test"))

(dap-register-debug-template
 "RB Server :: Dev"
 (list :type "python"
       :cwd "/Users/leo/dev/research-buddy/"
       ;; :module "pytest"
       :request "launch"
       :env '(
	      ("PYTHONPATH" . "/Users/leo/dev/research-buddy/monolith")
              ("API_ENV" . "dev")
              ("YT_SCRIPT_DIR" . "/Users/leo/dev/research-buddy/yt-script-db/development")
	      )
					;:pythonPath "/Users/leo/miniconda3/bin/python3 -Xfrozen_modules=off"
       :args ["-Xfrozen_modules=off"]  ; or try as a list '("-Xfrozen_modules=off")
       :debugger 'debugpy
       :name "RB Server :: Dev"))


;; (dap-register-debug-template
;;  "RB Server :: Dev foo"
;;  (list :type "python"
;;        :cwd "/Users/leo/dev/research-buddy/"
;;        ;; :module "pytest"
;;        :request "launch"
;;        :env '(("PYTHONPATH" . "/Users/leo/dev/research-buddy")
;;               ("API_ENV" . "dev")
;;               ("YT_SCRIPT_DIR" . "/Users/leo/dev/research-buddy/yt-script-db/development")
;; 	      )
;; 					;:pythonPath "/Users/leo/miniconda3/bin/python3 -Xfrozen_modules=off"
;;        :args ["-Xfrozen_modules=off"]  ; or try as a list '("-Xfrozen_modules=off")
;;        :debugger 'debugpy
;;        :name "RB Server :: Dev foo"))

;; PYTHONPATH=$(PYTHONPATH):./monolith/ API_ENV=test python monolith/app.py
