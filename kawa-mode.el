(require 'derived)

(defconst kawa--communication-buffer "*Kawa process*")

(defvar kawa-command "kawa")

(define-derived-mode kawa-mode scheme-mode "Kawa" "Major mode for editing Kawa files")

(defvar kawa-process nil
  "Kawa-bound process")

(defun create-kawa-process (&optional log)
  ;(start-process "Kawa" kawa--communication-buffer kawa-command)
  (make-process :name "Kawa"
                :buffer kawa--communication-buffer
                :command (list "kawa" "--console")
                :stderr (get-buffer-create "LOGBUFFER")))

(defun kawa--get-process ()
  (if (not (process-live-p kawa-process))
      (setq kawa-process (create-kawa-process)))
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
