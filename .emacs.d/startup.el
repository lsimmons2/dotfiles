;;(setq initial-buffer-choice "~/.emacs.d/init.el")
;;(setq initial-buffer-choice "~/dev/")
;;(setq initial-buffer-choice "~/dev/platz/frontend/src/components/GoogledLyricsSource.tsx")
;;(setq initial-buffer-choice "~/dev/platz/extension/platz-browser-extension/src/ScrapeGoogledLyricsContentScript.tsx")
;;(setq initial-buffer-choice "~/dev/crossword-project/cws/lib/http_client.ml")
;; (setq initial-buffer-choice "~/.emacs.d/startup.el")
;; (setq initial-buffer-choice "~/dev/org/org_server/lib/controller.ml")
;; (setq initial-buffer-choice "/Users/leo/dev/ocaml-playground/debugging_test/fib.ml")
(setq initial-buffer-choice "/Users/leo/dev/research-buddy/monolith/app.py")




;; File to save frame geometry
(defvar my-frame-size-file
  (expand-file-name "frame-size.el" user-emacs-directory)
  "File to save the Emacs frame's size and position.")

(defun my-save-frame-size ()
  "Save the current frame's size and position to `my-frame-size-file`."
  (interactive) ;; Add this line to make it accessible via M-x
  (let ((frame-geometry (list (frame-parameter nil 'left)
                              (frame-parameter nil 'top)
                              (frame-parameter nil 'width)
                              (frame-parameter nil 'height))))
    (message "[my-save-frame-size] Saving frame geometry: %S" frame-geometry)
    (with-temp-file my-frame-size-file
      (insert (format "%S" frame-geometry)))
    (message "[my-save-frame-size] Frame geometry saved to %s" my-frame-size-file)))

;; (1297 25 93 33)
(defun my-restore-frame-size ()
  "Restore the frame's size and position from `my-frame-size-file`."
  ;; Declare this as an interactive function so it can be called via M-x.
  (interactive)

  ;; Check if the file storing the frame geometry exists.
  (if (file-exists-p my-frame-size-file)
      ;; The file exists, so we attempt to restore the frame size.
      (let ((frame-geometry
             ;; Read the frame geometry from the file.
             ;; `with-temp-buffer` creates a temporary buffer that exists only for the duration of this block.
             (with-temp-buffer
               ;; Insert the contents of the frame size file into the temporary buffer.
               (insert-file-contents my-frame-size-file)
               ;; Read the buffer's content as a Lisp object (a list, in this case).
               (read (buffer-string)))))

        ;; Log the geometry we are attempting to restore for debugging.
        (message "[my-restore-frame-size] Restoring frame geometry: %S" frame-geometry)

        ;; Check if the geometry is valid before applying it.
        ;; `and` ensures all conditions are true before proceeding.
        (when (and frame-geometry
                   ;; Validate each part of the geometry:
                   ;; Ensure `left`, `top`, `width`, and `height` are integers.
                   (integerp (nth 0 frame-geometry)) ;; Left
                   (integerp (nth 1 frame-geometry)) ;; Top
                   (integerp (nth 2 frame-geometry)) ;; Width
                   (integerp (nth 3 frame-geometry)) ;; Height
                   ;; Ensure width and height are reasonable values (to avoid tiny or corrupted frames).
                   (> (nth 2 frame-geometry) 20)     ;; Width > 20 columns
                   (> (nth 3 frame-geometry) 10))    ;; Height > 10 rows

          ;; Apply the saved position to the current frame.
          ;; `nth` retrieves the nth element from the list:
          ;; - `nth 0` is the left position (X coordinate),
          ;; - `nth 1` is the top position (Y coordinate).
          (set-frame-position nil (nth 0 frame-geometry) (nth 1 frame-geometry))

          ;; Apply the saved size to the current frame.
          ;; - `nth 2` is the width (in columns),
          ;; - `nth 3` is the height (in rows).
          (set-frame-size nil (nth 2 frame-geometry) (nth 3 frame-geometry))

          ;; Log a success message for debugging.
          (message "[my-restore-frame-size] Frame geometry restored successfully.")))

    ;; If the file doesn’t exist, log a message indicating restoration is skipped.
    (message "[my-restore-frame-size] No frame size file found at %s. Skipping restoration."
             my-frame-size-file)))

;; Hook to save frame size on exit
(add-hook 'kill-emacs-hook
          (lambda ()
            (message "[kill-emacs-hook] Saving frame size before exiting...")
            (my-save-frame-size)))

;; Hook to restore frame size on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "[emacs-startup-hook] Attempting to restore frame size...")
            (my-restore-frame-size)))




;; Enable desktop-save-mode to save/restore session
;; (desktop-save-mode 1)

;; ;; Save the desktop session in your `.emacs.d/` directory
;; (setq desktop-path (list user-emacs-directory)) ;; Store in ~/.emacs.d/
;; (setq desktop-dirname user-emacs-directory)

;; ;; Ensure window layouts (including associated buffers) are restored
;; (setq desktop-restore-frames t)

;; ;; Save the desktop automatically on exit
;; (setq desktop-save t)

;; ;; Eagerly restore 10 buffers (adjust based on preference)
;; (setq desktop-restore-eager 10)

;; ;; Load the desktop even if it is locked (e.g., Emacs was previously killed)
;; (setq desktop-load-locked-desktop t)

;; ;; Save all file buffers
;; (setq desktop-files-not-to-save "^$")

;; ;; Increase minibuffer history length
;; (setq history-length 250)

;; ;; Save global state, including modes and variables
;; (setq desktop-globals-to-save
;;       '(desktop-missing-file-warning
;;         tags-table-list
;;         register-alist
;;         file-name-history
;;         kill-ring
;;         search-ring
;;         regexp-search-ring))
