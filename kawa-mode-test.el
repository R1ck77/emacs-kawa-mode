(require 'buttercup)
(require 'seq)
(setq load-path (cons "." load-path))
(require 'kawa-mode-utils)
(require 'kawa-mode)

(defconst debug-message "Form correctly interpreted!")

(defun find-kawa-process ()
  (car
   (seq-filter (lambda (x)
                 (eq (get-buffer "*Kawa process*")
                     (process-buffer x)))
               (process-list))))

(defun kill-kawa-processes ()
  (when-let ((process (find-kawa-process)))
    (process-kill-without-query process)
    (kill-buffer "*Kawa process*")))

(defun read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring (point-min) (point-max))))

(describe "kawa-mode.el"
  :var (temp-file)
  (after-each
    (kill-kawa-processes))
  (describe "kawa-mode"
    (it "sets the current buffer's mode and name"
      (with-temp-buffer
        (kawa-mode)
        (expect mode-name
                :to-equal "Kawa")
        (expect major-mode
                :to-equal 'kawa-mode))))
  (describe "kawa-start"
    (it "starts a buffer with a process"
      (expect (not (find-kawa-process)))
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)
        (expect (process-live-p (find-kawa-process)))))
    (it "does not start a process is one is present already"
      (expect (not (find-kawa-process)))
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)
        (let ((old-process (find-kawa-process)))
          (expect (process-live-p (find-kawa-process)))
          (kawa-start)
          (expect (eq old-process (find-kawa-process)))))))
  (describe "kawa-send-buffer"
    (it "starts the kawa interpreter if it's not present already"
      (expect (not (find-kawa-process)))
      (with-temp-buffer
        (insert "(define x \"x symbol's value\"")
        (kawa-mode)
        (kawa-send-buffer)
        (expect (process-live-p (find-kawa-process)))))
    (it "sends the buffer to the process"
      (expect (not (find-kawa-process)))
      (setq temp-file (make-temp-file "kawa-test-"))
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)       
        (with-temp-buffer
          (let ((content (concat "(set! &<{" temp-file "} \"wrong message\")\n"
                                 "(set! &<{" temp-file "} \"" debug-message "\")")))
            (insert content))
          (kawa-send-buffer)))
      (with-current-buffer "LOGBUFFER"
        (message "Error content: %s\n" (buffer-substring-no-properties (point-min) (point-max))))
      (sit-for 1)
      (expect (read-file temp-file)
              :to-equal debug-message))))
