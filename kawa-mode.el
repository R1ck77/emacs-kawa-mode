(require 'derived)
(require 'cl)
(require 'subr-x)
(require 'kawa-mode-utils)

(defconst kawa-buffer-name "*Kawa REPL*")

(defconst kawa-command "kawa")

(defconst kawa-output-timeout 60)

(defvar kawa-process nil
  "Kawa-bound process")

(defvar kawa-mode-map nil
  "Keymap for Kawa major mode")
(unless kawa-mode-map
  (setq kawa-mode-map (make-sparse-keymap))
  (define-key kawa-mode-map "\C-c\M-j" 'kawa-start)  
  (define-key kawa-mode-map "\C-x\C-k" 'kawa-eval-buffer)
  (define-key kawa-mode-map "\C-x\C-e" 'kawa-eval-expr-at-point))

(defvar kawa--output-received nil
  "t when the filter just received some contents")
(make-variable-buffer-local 'kawa--output-received)

(defun kawa--move-cursor-to-end (buffer)
  (mapc (lambda (window)
          (set-window-point window (point-max)))
        (get-buffer-window-list buffer)))

(defun kawa--filter (process content)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert content)
      (set-marker (process-mark process) (point-max))
      (kawa--move-cursor-to-end (current-buffer))
      (setq kawa--output-received t))))

(defun kawa-wait-for-output (&optional timeout)
  "Wait for the kawa process to return some output"
  (kawa--while-with-timeout (lambda ()
                              (with-current-buffer (process-buffer kawa-process)
                                (and (not kawa--output-received)
                                     (process-live-p kawa-process))))
                            (or timeout kawa-output-timeout))
  (with-current-buffer (process-buffer kawa-process)
    (setq kawa--output-received nil)))

(defun kawa--create-kawa-process (&optional log)
  (let ((process (make-process :name "Kawa"
                               :command (list kawa-command "--console")
                               :buffer kawa-buffer-name
                               :connection-type 'pipe
                               :filter 'kawa--filter)))
    (set-process-sentinel process (lambda (p s)))
    (or (process-live-p process)
        (error "Kawa process not started (ensure the command \"Kawa\" is in your PATH)!"))
    process))

(defun kawa--setup-repl-buffer ()
  (with-current-buffer kawa-buffer-name
    (erase-buffer)
    (kill-all-local-variables)
    (kawa-wait-for-output)
    (display-buffer (current-buffer))))

(defun kawa--get-process ()
  (when (not (process-live-p kawa-process))
    (setq kawa-process (kawa--create-kawa-process))
    (kawa--setup-repl-buffer))
  kawa-process)

(defun kawa-start ()
  (interactive)
  (kawa--get-process))

(defun kawa--expression-feedback (content)
  (with-current-buffer (process-buffer kawa-process)
    (goto-char (process-mark kawa-process))
    (insert (format "%s\n" content))
    (set-marker (process-mark kawa-process) (point-max))))

;;; TODO/FIXME is there a point in using the process variable instead of kawa--get-process
(defun kawa-eval-buffer ()
  (interactive)
  (kawa-start)
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))    
    (process-send-string kawa-process "(begin\n")
    (process-send-string kawa-process content)
    (process-send-string kawa-process ")\n")
    (kawa-wait-for-output)))

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

(defun kawa--eval-expr (content)
  (process-send-string kawa-process content)
  (process-send-string kawa-process "\n")
  (kawa-wait-for-output))

;;; TODO/FIXME shouldn't I check if a newline is there already?
(defun kawa-eval-expr-at-point ()
  (interactive)
  (kawa-start)
  (let ((content (apply 'buffer-substring-no-properties (kawa--previous-expression-bounds))))
    (kawa--expression-feedback content)
    (kawa--eval-expr content)))

;;; TODO/FIXME check for correct buffer!
(defun kawa-return ()
  (interactive)
  (with-current-buffer kawa-buffer-name
    (let ((content (buffer-substring-no-properties (process-mark kawa-process) (point-max))))
      (message "evaluating  '%s'" content)
      (insert "\n")
      (kawa--eval-expr content))))

(define-derived-mode kawa-mode scheme-mode
  "Kawa" "Major mode for editing Kawa files.

Special commands:
\\{kawa-mode-map}"
  (setq kawa-process nil)
  (setq kawa-output-received nil))

(provide 'kawa-mode)
