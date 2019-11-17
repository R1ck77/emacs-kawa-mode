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

(defun time ()
  (let ((time (current-time)))
    (+ (elt time 1) (/ (elt time 2) 1e6))))

(defun wait-for-kawa-to-exit-with-timeout (timeout)
  (let ((start (time)))
    (while (process-live-p (find-kawa-process))
      (sit-for 0.1)
      (if (> (- (time) start) timeout)
          (error "Timeout waiting for kawa to finish!")))))

(defun remove-file-if-any (path)
  (condition-case nil
      (delete-file path)
    (error nil)))

(describe "kawa-mode.el"
  :var (temp-file)
  (before-each
    (setq temp-file nil))
  (after-each
    (kill-kawa-processes)
    (when temp-file
      (remove-file-if-any temp-file)))
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
          (expect (eq old-process (find-kawa-process))))))
    (it "creates a new buffer and displays it"
      (with-temp-buffer
        (spy-on 'display-buffer)
        (kawa-mode)
        (kawa-start)
        (expect (get-buffer "*Kawa REPL*"))
        (expect (spy-calls-all-args 'display-buffer)
                :to-equal (list (list (get-buffer "*Kawa REPL*")))))))
  (describe "kawa-eval-expr-at-point"
    (it "is a command"
      (expect (commandp 'kawa-send-buffer)))    
    (it "starts the kawa interpreter if one is not running already"
      (expect (not (find-kawa-process)))
      (with-temp-buffer
        (insert "\(define x \"x symbol's value\"\)")
        (kawa-eval-expr-at-point))
      (expect (process-live-p (find-kawa-process))))
    (it "throws an error if there is no expression before the point"
      (expect (not (find-kawa-process)))
      (with-temp-buffer
        (insert (make-string 100 ? ))
        (expect (kawa-eval-expr-at-point) :to-throw 'error)))
    (it "throws an error if there is no expression before the point (even if there is an expression after the point)"
      (expect (not (find-kawa-process)))
      (with-temp-buffer
        (insert (make-string 100 ? ))
        (insert "\(define x 12\)")
        (goto-char 50)
        (expect (kawa-eval-expr-at-point) :to-throw 'error)))
  (describe "kawa-send-buffer"
    (it "is a command"
      (expect (commandp 'kawa-send-buffer)))
    (it "starts the kawa interpreter if it not running already"
      (expect (not (find-kawa-process)))
      (with-temp-buffer
        (insert "\(define x \"x symbol's value\"\)")
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
          (insert (concat "(set! &<{" temp-file "} \"wrong message\")"
                          "(set! &<{" temp-file "} \"" debug-message "\")"
                          "(exit 0)"))
          (kawa-send-buffer)))
      (wait-for-kawa-to-exit-with-timeout 5) ;; TODO/FIXME This is ugly™
      (expect (read-file temp-file)
              :to-equal debug-message))))
    (describe "kawa REPL buffer"
    (it "shows the kawa prompt in the buffer"
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)
        (with-current-buffer (get-buffer "*Kawa REPL*")
          (sit-for 2) ;;; TODO/FIXME quick dirty fix
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "#|kawa:1|# "))))))
