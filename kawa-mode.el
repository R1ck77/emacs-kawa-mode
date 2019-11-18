(require 'derived)
(require 'cl)
(require 'subr-x)

(defconst kawa--communication-buffer "*Kawa process*")
(defconst kawa-buffer-name "*Kawa REPL*")

(defvar kawa-command "kawa")

(define-derived-mode kawa-mode scheme-mode "Kawa" "Major mode for editing Kawa files")

(defvar kawa-process nil
  "Kawa-bound process")

(defun kawa--setup-repl-buffer-hooks ()
  (with-current-buffer (get-buffer-create kawa--communication-buffer)        
    (add-hook 'after-change-functions
              (lambda (start end length)
                (kawa--copy-process-buffer-content))
              t t)))

(defun create-kawa-process (&optional log)
  (let ((process-connection-type nil))
    (get-buffer-create kawa-buffer-name)
    (kawa--setup-repl-buffer-hooks)
    (make-process :name "Kawa"
                  :command (list kawa-command "--console")
                  :buffer kawa--communication-buffer
                  :filter (lambda (process content)
                            (when (buffer-live-p (process-buffer process))
                              (print (format "***** filtering %s…" content))
                              (with-current-buffer (process-buffer process)
                                (insert content)))))))

(defun kawa--cut-communication-buffer-content ()
  (with-current-buffer kawa--communication-buffer
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (erase-buffer)
      (set-marker (process-mark kawa-process) (point-max))
      content)))

(defun kawa--copy-process-buffer-content ()
  (with-current-buffer kawa-buffer-name
    (goto-char (point-max))
    (insert (kawa--cut-communication-buffer-content))))

(defun kawa--setup-repl-buffer ()
  (with-current-buffer kawa-buffer-name
    (erase-buffer)
    (kill-all-local-variables)
    (kawa--setup-repl-buffer-hooks)
    (display-buffer (current-buffer))))

(defun kawa--get-process ()
  (when (not (process-live-p kawa-process))
    (setq kawa-process (create-kawa-process))
    (kawa--setup-repl-buffer))
  kawa-process)

(defun kawa-start ()
  (interactive)
  (kawa--get-process))

(defun kawa--expression-feedback (content)
  (with-current-buffer kawa-buffer-name
    (print (format "current content: %s, current buffer: %s" content (current-buffer)))
    (insert (format "%s\n" content))))

;;; TODO/FIXME is there a point in using the process variable instead of kawa--get-process
(defun kawa-eval-buffer ()
  (interactive)
  (kawa-start)
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (kawa--expression-feedback content)
    (process-send-string kawa-process content)
    (process-send-string kawa-process "\n")))

(defun kawa--raw-previous-expression-bounds ()
  (save-excursion    
    (backward-sexp)
    (let ((from (point)))
      (forward-sexp)
      (list from (point)))))

(defun kawa--valid-bounds-p (bounds)
  (let ((trimmed-content (string-trim (apply 'buffer-substring-no-properties bounds))))
    (and (> (length trimmed-content) 0)
         (<= (second bounds) (point)))))

(defun kawa--previous-expression-bounds ()
  "Returns a list with the beginning and end of the last sexp"
  (let ((bounds (kawa--raw-previous-expression-bounds)))
    (if (kawa--valid-bounds-p bounds)
        bounds
      (error "No expression at point"))))

(defun kawa-eval-expr-at-point ()
  (interactive)
  (kawa-start)
  (apply 'process-send-region (cons kawa-process (kawa--previous-expression-bounds)))
  (process-send-string kawa-process "\n")) ; TODO/FIXME shouldn't I check if a newline is there already?

(provide 'kawa-mode)
