(require 'derived)

(defconst kawa--communication-buffer "*Kawa process*")

(defvar kawa-command "kawa")

(define-derived-mode kawa-mode scheme-mode "Kawa" "Major mode for editing Kawa files")

(defun kawa-start ()
  (interactive)
  (start-process "Kawa" kawa--communication-buffer kawa-command))

(defun kawa-send-buffer ()
  (interactive)
  (kawa-start))

(provide 'kawa-mode)
