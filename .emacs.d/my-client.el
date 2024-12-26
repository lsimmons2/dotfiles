(require 'request)
(require 'tabulated-list)

;; (base) ~/dev/diddling % curl localhost:8000/api/things
;; [{"id":1,"name":"Thing 1","status":"active"},{"id":2,"name":"Thing 2","status":"inactive"},{"id":3,"name":"Thing 3","status":"pending"}]%


(defvar my-client-api-url "http://localhost:8000/api/things"
  "The base URL of the server API.")

(defvar my-client-data nil
  "Cached data from the server.")

(defun my-function ()
  (interactive)
  (message "Hello, world!"))


(defun ping-api ()
  (interactive)
  (let ((ping-url (format "%s/ping" my-client-api-url)))
    (message "Calling url %s" my-client-api-url)
    (request
      ping-url
      :type "GET"
      :parser 'buffer-string
      :success (cl-function
		(lambda (&key data &allow-other-keys)
                  (message "Response: %s" data)))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
		(message "Error: %s" error-thrown))))))

(defun my-client-fetch-data ()
  "Fetch the list of things from the server."
  (interactive)
  (message "Calling url %s" my-client-api-url)
  (request
    my-client-api-url
    :type "GET"
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(setq my-client-data data)
		(my-client-refresh-ui)))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error fetching data: %S" error-thrown)))))



(defun my-client-perform-action (thing-id)
  "Perform an action on the thing with THING-ID."
  (let ((thing-id thing-id)) ;; Explicitly capture the value
    (message "thing-id in my-client-perform-action is %s" thing-id)
    (request
      (format "%s/%s/action" my-client-api-url thing-id)
      :type "POST"
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
                  (message "Action performed on thing %s!" thing-id)
                  (my-client-fetch-data))) ;; Refresh data after success
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
		(message "Error performing action on thing %s: %S"
                         thing-id error-thrown)))))))

(defun my-client-refresh-ui ()
  "Refresh the UI with the latest data."
  (with-current-buffer (get-buffer-create "*My Client*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (tabulated-list-mode)
      (setq tabulated-list-format [("ID" 10 t)
                                   ("Name" 20 t)
                                   ("Status" 10 t)])
      (setq tabulated-list-entries
	    (mapcar (lambda (thing)
		      (let ((id (alist-get 'id thing))
			    (name (alist-get 'name thing))
			    (status (alist-get 'status thing)))
                        (list id
			      (vector (format "%s" id)
				      name
				      status))))
		    my-client-data))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (my-client-setup-keybindings))
    (switch-to-buffer "*My Client*")))


(defun my-client-setup-keybindings ()
  "Set up keybindings for the UI."
  (define-key tabulated-list-mode-map (kbd "C-a")
	      (lambda ()
		(interactive)
		(message "thing-id in my-client-setup-keybindings is %s" thing-id)
		(let* ((thing-id (tabulated-list-get-id))) ;; Capture thing-id here
		  (when thing-id
		    (my-client-perform-action thing-id))))))

(defun my-client-start ()
  "Start the client by fetching data and opening the UI."
  (interactive)
  (my-client-fetch-data))
