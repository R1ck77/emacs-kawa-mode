(require 'derived)

(defconst kawa--communication-buffer "*Kawa process*")

(defvar kawa-command "kawa")

(define-derived-mode kawa-mode scheme-mode "Kawa" "Major mode for editing Kawa files")

(defvar kawa-process nil
  "Kawa-bound process")

(defun kawa-start ()
  (interactive)
  (if (not (process-live-p kawa-process))
      (setq kawa-process (start-process "Kawa"
                                        kawa--communication-buffer
                                        kawa-command))))

(defun kawa-send-buffer ()
  (interactive)
  (kawa-start))

(provide 'kawa-mode)
