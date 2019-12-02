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

(defvar kawa--last-command-index 0
  "counts the number of history properties added")
(make-variable-buffer-local 'kawa--last-command-index)

(defvar kawa--input-buffer "")
(make-variable-buffer-local 'kawa--input-buffer)

(defun kawa--move-cursor-to-end (buffer)
  (mapc (lambda (window)
          (set-window-point window (point-max)))
        (get-buffer-window-list buffer)))

(defun kawa--string-skip-prompt (text)
  (let ((start (string-match "#|\\(kawa:\\|\.\.\.\.\.\\)[0-9]+|# " text)))
    (and start (substring text (match-end 0) (length text)))))

;; TODO/FIXME this could consider the previous index of the prompt for added safety!
(defun kawa--ends-with-prompt-p (text)
  (let ((remaining-text (kawa--string-skip-prompt text)))
    (if remaining-text
        (or (= (length remaining-text) 0)
            (kawa--ends-with-prompt-p remaining-text)))))

(defun kawa--filter (process content)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (setq kawa--input-buffer (concat kawa--input-buffer content))
      (when (kawa--ends-with-prompt-p kawa--input-buffer)
        (goto-char (point-max))
        (insert kawa--input-buffer)
        (setq kawa--input-buffer "")
        (set-marker (process-mark process) (point-max))
        (kawa--move-cursor-to-end (current-buffer))
        (setq kawa--output-received t)))))

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
                               :filter #'kawa--filter)))
    (set-process-sentinel process (lambda (p s)))
    (or (process-live-p process)
        (error "Kawa process not started (ensure the command \"Kawa\" is in your PATH)!"))
    process)) 

(defun kawa--setup-repl-buffer ()
  (with-current-buffer kawa-buffer-name
    (erase-buffer)
    (kill-all-local-variables)
    (kawa-wait-for-output)
    (display-buffer (current-buffer))
    (local-set-key (kbd "RET") 'kawa-return)
    (local-set-key (kbd "M-p") 'kawa-history-prev)))

(defun kawa--get-process ()
  (when (not (process-live-p kawa-process))
    (setq kawa-process (kawa--create-kawa-process))
    (kawa--setup-repl-buffer))
  kawa-process)

(defun kawa-start ()
  "Start a Kawa REPL session if one is not present already"
  (interactive)
  (kawa--get-process))

;;; TODO/FIXME is there a point in using the process variable instead of kawa--get-process
(defun kawa-eval-buffer ()
  "Send the current buffer to the kawa process"
  (interactive)
  (kawa-start)
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))    
    (process-send-string kawa-process "(begin\n")
    (process-send-string kawa-process content)
    (process-send-string kawa-process ")\n")
    (kawa-wait-for-output)))

(defun kawa--eval-expr (content)
  (process-send-string kawa-process content)
  (process-send-string kawa-process "\n")
  (kawa-wait-for-output))

(defun kawa--add-history-property (from to)
  (put-text-property from to
                     'kawa-history-expression kawa--last-command-index)
  (setq kawa--last-command-index (+ 1 kawa--last-command-index))
  (put-text-property from to ;; TODO/FIXME debug only
                     'font-lock-face (list ':foreground "yellow")))

(defun kawa--expression-feedback (content)
  (with-current-buffer (process-buffer kawa-process)
    (goto-char (process-mark kawa-process))
    (insert content)
    (kawa--add-history-property (process-mark kawa-process) (point-max))
    (insert "\n")
    (set-marker (process-mark kawa-process) (point-max))))

(defun kawa--previous-expression-bounds ()
  "Returns a list with the beginning and end of the last sexp"
  (let ((bounds (kawa--raw-previous-expression-bounds)))
    (if (kawa--valid-bounds-p bounds)
        bounds
      (error "No expression at point"))))

(defun kawa--valid-bounds-p (bounds)
  (let ((trimmed-content (string-trim (apply #'buffer-substring-no-properties bounds))))
    (and (> (length trimmed-content) 0)
         (<= (second bounds) (point)))))

(defun kawa--raw-previous-expression-bounds ()
  (save-excursion    
    (backward-sexp)
    (let ((from (point)))
      (forward-sexp)
      (list from (point)))))

;;; TODO/FIXME shouldn't I check if a newline is there already?
(defun kawa-eval-expr-at-point ()
  "Send the expression before the point to the Kawa interpreter"
  (interactive)
  (kawa-start)
  (let ((content (apply #'buffer-substring-no-properties (kawa--previous-expression-bounds))))
    (kawa--expression-feedback content)
    (kawa--eval-expr content)))

(defun kawa-return ()
  "Send the current expression to the Kawa interpreter"
  (interactive)
  (if (eq (current-buffer) (get-buffer kawa-buffer-name))      
      (let ((content (buffer-substring-no-properties (process-mark kawa-process) (point-max))))
        (kawa--add-history-property (process-mark kawa-process) (point-max))
        (goto-char (point-max)) ;;; TODO/FIXME this one would require a test! If the cursor is before the expression the wrong newline is inserted
        (insert "\n")
        (kawa--eval-expr content))
    (error "kawa-return should be invoked only in the Kawa REPL")))

(defun kawa-history-prev ()
  "Replace the current expression in the repl with one of the previously evaluated"
  (interactive)
  (when (not (eq (process-buffer kawa-process) (current-buffer)))
    (error "This command can only be executed in the Kawa REPL"))
  (let ((history-command (kawa-previous-text-with-property 'kawa-history-expression)))
    (when (not history-command)
      (error "No elements in the history"))
    (save-excursion
      (kill-region (process-mark kawa-process) (point-max))
      (goto-char (point-max))
      (insert history-command))))

(define-derived-mode kawa-mode scheme-mode
  "Kawa" "Major mode for editing Kawa files.

Special commands:
\\{kawa-mode-map}"
  (setq kawa-process nil)
  (setq kawa-output-received nil))

(provide 'kawa-mode)
