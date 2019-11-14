(require 'buttercup)
(require 'seq)
(setq load-path (cons "." load-path))
(require 'kawa-mode)

(defun find-kawa-process ()
  (car
   (seq-filter (lambda (x)
                 (eq (get-buffer "*Kawa process*")
                     (process-buffer x)))
               (process-list))))

(defmacro comment (&forms))

(defmacro if-let (definition &rest forms)
  (declare (indent 1))
  "Allows to write:

(if-let ((var1 expr)
         (var2 expr))
    internal forms)

which will return nil without evaluating the internal forms, or the result of
the internal forms evaluation (where the bindings are visible) if var2 is true"
  `(let ,definition
     (if ,(car (car (reverse definition)))
         (progn ,@forms))))

(defun kill-kawa-processes ()
  (if-let ((process (find-kawa-process)))
    (process-kill-without-query process)
    (kill-buffer "*Kawa process*")))

(describe "kawa-mode.el"
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
    (xit "sends the buffer to the process"
      (expect (not (find-kawa-process)))
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)
        ))))
