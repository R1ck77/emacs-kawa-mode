(require 'derived)
(require 'cl)

(defconst kawa--communication-buffer "*Kawa process*")
(defconst kawa-buffer-name "*Kawa REPL*")

(defvar kawa-command "kawa")

(define-derived-mode kawa-mode scheme-mode "Kawa" "Major mode for editing Kawa files")

(defvar kawa-process nil
  "Kawa-bound process")

(defun create-kawa-process (&optional log)
  (start-process "Kawa" kawa--communication-buffer kawa-command))

(defun kawa--copy-process-buffer-content ()
  (with-current-buffer (get-buffer kawa--communication-buffer)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (assert (> (length content) 0))
      (with-current-buffer (get-buffer kawa-buffer-name)
        (goto-char (point-max))
        (insert content)))
    (erase-buffer)))

(defun kawa--setup-repl-buffer-hooks ()
  (with-current-buffer (get-buffer kawa--communication-buffer)        
    (add-hook 'after-change-functions
              (lambda (start end length)
                (kawa--copy-process-buffer-content))
              t t)))

(defun kawa--setup-repl-buffer ()
  (with-current-buffer (get-buffer-create kawa-buffer-name)
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

;;; TODO/FIXME is there a point in using the process variable instead of kawa--get-process
(defun kawa-send-buffer ()
  (interactive)
  (kawa-start)
  (process-send-region kawa-process (point-min) (point-max))
  (process-send-string kawa-process "\n"))

(provide 'kawa-mode)
