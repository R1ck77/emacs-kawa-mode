(require 'buttercup)
(require 'seq)
(require 'kawa-mode-utils)
(require 'kawa-mode)

(defconst debug-message "Form correctly interpreted!")

(defun find-kawa-process ()
  (car
   (seq-filter (lambda (x)
                 (eq (get-buffer "*Kawa REPL*")
                     (process-buffer x)))
               (process-list))))

(defun kill-kawa-processes ()
  (when-let ((process (find-kawa-process)))
    (process-kill-without-query process)
    (kill-buffer "*Kawa REPL*")))

(defun read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring (point-min) (point-max))))

(defun wait-for-kawa-to-exit-with-timeout (timeout)
  (let ((process (find-kawa-process)))
    (kawa--while-with-timeout (lambda ()
                                (process-live-p process))
                              timeout)))

(defun wait-for-kawa-buffer-to-change-with-timeout (timeout)
  (let ((starting-size (point-max)))
   (kawa--while-with-timeout (lambda ()
                               (= (point-max) starting-size))
                             timeout)))

(defun remove-file-if-any (path)
  (condition-case nil
      (delete-file path)
    (error nil)))

(describe "kawa-mode.el"
  :var (temp-file)
  (before-each
    (setq temp-file nil)
    (expect (not (find-kawa-process))))
  (after-each
    (kill-kawa-processes)
    (when temp-file
      (remove-file-if-any temp-file)))
  (describe "kawa-mode"
    (it "is a command"
      (expect (commandp 'kawa-mode)))
    (it "sets the current buffer's mode and name"
      (with-temp-buffer
        (kawa-mode)
        (expect mode-name
                :to-equal "Kawa")
        (expect major-mode
                :to-equal 'kawa-mode))))
  (describe "kawa-start"
    (it "is a command"
      (expect (commandp 'kawa-start)))
    (it "starts a buffer with a process"
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)
        (expect (process-live-p (find-kawa-process)))))
    (it "does not start a process is one is present already"
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
      (expect (commandp 'kawa-eval-expr-at-point)))    
    (it "starts the kawa interpreter if one is not running already"
      (with-temp-buffer
        (kawa-mode)
        (insert "\(define x \"x symbol's value\"\)")
        (kawa-eval-expr-at-point))
      (expect (process-live-p (find-kawa-process))))
    (it "throws an error if there is no expression before the point"
      (with-temp-buffer
        (kawa-mode)
        (insert (make-string 100 ? ))
        (expect (kawa-eval-expr-at-point) :to-throw 'error)))
    (it "throws an error if there is no expression before the point (even if there is an expression after the point)"
      (with-temp-buffer
        (kawa-mode)
        (insert (make-string 100 ? ))
        (insert "\(define x 12\)")
        (goto-char 50)
        (expect (kawa-eval-expr-at-point) :to-throw 'error)))
    (it "evaluates the expression in the buffer"
      (with-temp-buffer
        (insert "\(exit 0\) \(exit 3\)  ")
        (kawa-mode)
        (kawa-start)
        (let ((process (find-kawa-process)))
          (kawa-eval-expr-at-point)
          (expect (process-exit-status process)
                  :to-be 3))))
    (it "sends the expression into the REPL"
      (with-temp-buffer        
        (insert "\(define x 12\)")
        (kawa-mode)
        (kawa-eval-expr-at-point)
        (insert "x")
        (kawa-eval-expr-at-point)
        (with-current-buffer (get-buffer "*Kawa REPL*")
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "#|kawa:1|# \(define x 12\)\n#|kawa:2|# x\n12\n#|kawa:3|# "))))
    (it "positions the cursor at the end of the buffer"
      (with-temp-buffer        
        (insert "\(define x 12\)")
        (kawa-mode)
        (kawa-eval-expr-at-point)
        (let ((windows (get-buffer-window-list "*Kawa REPL*"))
              (position (with-current-buffer "*Kawa REPL*"
                          (point-max))))
          (expect (length windows) :not :to-be 0)
          (expect (window-point (car windows)) :to-be position)))))
  (describe "kawa-eval-buffer"
    (it "is a command"
      (expect (commandp 'kawa-eval-buffer)))
    (it "starts the kawa interpreter if it not running already"
      (with-temp-buffer
        (insert "\(define x \"x symbol's value\"\)")
        (kawa-mode)
        (kawa-eval-buffer)
        (expect (process-live-p (find-kawa-process)))))
    (it "sends the buffer to the process"
      (setq temp-file (make-temp-file "kawa-test-"))
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)
        (let ((process (find-kawa-process)))          
          (with-temp-buffer
            (insert (concat "(set! &<{" temp-file "} \"wrong message\")"
                            "(set! &<{" temp-file "} \"" debug-message "\")"
                            "(exit 4)"))
            (kawa-eval-buffer))
          (wait-for-kawa-to-exit-with-timeout 5)
          (expect (process-exit-status process)
                  :to-be 4)
          (expect (read-file temp-file)
                  :to-equal debug-message)))))
  (describe "kawa-return"
    (it "evaluates the empty expression and returns appropriate feedback"
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)
        (with-current-buffer (get-buffer "*Kawa REPL*")
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "#|kawa:1|# ")
          (kawa-return)
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "#|kawa:1|# \n#|kawa:2|# "))))
    (it "evaluates the current expression and return feedback"
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)
        (with-current-buffer (get-buffer "*Kawa REPL*")
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "#|kawa:1|# ")
          (insert "(define x 12)")
          (kawa-return)
          (insert "x")
          (kawa-return)
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "#|kawa:1|# (define x 12)\n#|kawa:2|# x\n12\n#|kawa:3|# "))))
    (it "evaluates multi-line expressions as well"
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)
        (with-current-buffer (get-buffer "*Kawa REPL*")
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "#|kawa:1|# ")
          (insert "(define x")
          (kawa-return)
          (insert "12)")
          (kawa-return)
          (insert "x")
          (kawa-return)
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "#|kawa:1|# (define x\n#|.....2|# 12)\n#|kawa:3|# x\n12\n#|kawa:4|# ")))))
  (describe "kawa-history-prev"
    (it "raises an error if in the wrong buffer"
      (with-temp-buffer
        (kawa-mode)
        (with-temp-buffer
          (kawa-mode)
          (kawa-start)
          (expect (kawa-history-prev) :to-throw 'error))))
    (it "raises an error if there is no previous expression"
      (with-temp-buffer
        (kawa-mode)
        (with-temp-buffer
          (kawa-mode)
          (kawa-start)
          (with-current-buffer (get-buffer "*Kawa REPL*")
            (expect (kawa-history-prev) :to-throw 'error)))))
    (it "replaces the current expression in the REPL with the previously evaluated one (kawa-eval-expr-at-point case)"
      (with-temp-buffer
        (kawa-mode)
        (with-temp-buffer
          (kawa-mode)
          (kawa-start)
          (insert "(define x 12)")
          (kawa-eval-expr-at-point)
          (with-current-buffer (get-buffer "*Kawa REPL*")
            (insert "x")
            (kawa-history-prev)
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-equal "#|kawa:1|# (define x 12)
#|kawa:2|# (define x 12)")))))
    (it "replaces the current expression in the REPL with the previously evaluated one (kawa-return case)"
      (with-temp-buffer
        (kawa-mode)
        (with-temp-buffer
          (kawa-mode)
          (kawa-start)
          (with-current-buffer (get-buffer "*Kawa REPL*")
            (insert "(define x 12)")
            (kawa-return)
            (insert "x")
            (kawa-history-prev)
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-equal "#|kawa:1|# (define x 12)
#|kawa:2|# (define x 12)")))))
    (it "puts the cursor at the end of the expression just substituted"
      (with-temp-buffer
        (kawa-mode)
        (with-temp-buffer
          (kawa-mode)
          (kawa-start)
          (with-current-buffer (get-buffer "*Kawa REPL*")
            (insert "(define x 12)")
            (kawa-return)
            (kawa-history-prev)
            (expect (point)
                    :to-equal (point-max))))))
    (it "takes into consideration changes made to the previous expression when replacing"
      (with-temp-buffer
        (kawa-mode)
        (with-temp-buffer
          (kawa-mode)
          (kawa-start)
          (with-current-buffer (get-buffer "*Kawa REPL*")
            (switch-to-buffer (current-buffer))
            (insert "(define x 12)")
            (let ((end-of-expression (point)))              
              (kawa-return)
              (goto-char (- end-of-expression 1))
              (setq last-kbd-macro "00")
              (kmacro-end-and-call-macro 1)
              (goto-char (point-max))
              (kawa-history-prev)
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "#|kawa:1|# (define x 1200)
#|kawa:2|# (define x 1200)"))))))
    (it "can be repeated to repeat older commands"
      (with-temp-buffer
        (kawa-mode)
        (with-temp-buffer
          (kawa-mode)
          (kawa-start)
          (with-current-buffer (get-buffer "*Kawa REPL*")
            (insert "(define x 12)")
            (kawa-return)
            (insert "(define y 14)")
            (kawa-return)
            (insert "(define x 44)")
            (kawa-return)
            (kawa-history-prev) ; (define x 44)
            (kawa-history-prev) ; (define y 14)
            (kawa-history-prev) ; (define x 12)
            (kawa-return)
            (insert "x")
            (kawa-return)
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-equal "#|kawa:1|# (define x 12)
#|kawa:2|# (define y 14)
#|kawa:3|# (define x 44)
#|kawa:4|# (define x 12)
#|kawa:5|# x
12
#|kawa:6|#")))))
    (it "can be used with a prefix"
      (with-temp-buffer
        (kawa-mode)
        (with-temp-buffer
          (kawa-mode)
          (kawa-start)
          (with-current-buffer (get-buffer "*Kawa REPL*")
            (insert "(define x 12)")
            (kawa-return)
            (insert "(define y 14)")
            (kawa-return)
            (insert "(define x 44)")
            (kawa-return)
            (kawa-history-prev 3)
            (kawa-return)
            (insert "x")
            (kawa-return)
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-equal "#|kawa:1|# (define x 12)
#|kawa:2|# (define y 14)
#|kawa:3|# (define x 44)
#|kawa:4|# (define x 12)
#|kawa:5|# x
12
#|kawa:6|#")))))
    (it "raises an error if invoked more than the commands available"
      (with-temp-buffer
        (kawa-mode)
        (with-temp-buffer
          (kawa-mode)
          (kawa-start)
          (with-current-buffer (get-buffer "*Kawa REPL*")
            (insert "(define x 12)")
            (kawa-return)
            (insert "(define y 14)")
            (kawa-return)
            (kawa-history-prev)
            (kawa-history-prev)
            (expect (kawa-history-prev)
                    :to-throw 'error)))))
    (it "raises an error if invoked with an argument greater than the prefix"
      (with-temp-buffer
        (kawa-mode)
        (with-temp-buffer
          (kawa-mode)
          (kawa-start)
          (with-current-buffer (get-buffer "*Kawa REPL*")
            (insert "(define x 12)")
            (kawa-return)
            (insert "(define y 14)")
            (kawa-return)
            (expect (kawa-history-prev 12)
                    :to-throw 'error)))))
    (it "raises an error if invoked with a negative argument"
      (with-temp-buffer
        (kawa-mode)
        (with-temp-buffer
          (kawa-mode)
          (kawa-start)
          (with-current-buffer (get-buffer "*Kawa REPL*")
            (insert "(define x 12)")
            (kawa-return)
            (insert "(define y 14)")
            (kawa-return)
            (expect (kawa-history-prev -1)
                    :to-throw 'error)))))))

(describe "kawa REPL buffer"
  (it "shows the kawa prompt in the buffer"
    (with-temp-buffer
      (kawa-mode)
      (kawa-start)
      (with-current-buffer (get-buffer "*Kawa REPL*")
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "#|kawa:1|# "))))
    (it "RET is equivalent to kawa-return in the REPL"
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)
        (with-current-buffer (get-buffer "*Kawa REPL*")
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "#|kawa:1|# ")
          (expect (local-key-binding (kbd "RET"))
                  :to-be 'kawa-return)))))
