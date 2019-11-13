(require 'buttercup)
(require 'seq)
(setq load-path (cons "." load-path))
(require 'kawa-mode)

(defun find-kawa-process ()
  (car
   (seq-filter (lambda (x) (eq (get-buffer "*Kawa process*") x))
               (mapcar 'process-buffer (process-list)))))

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
    (print "Process found")
    (process-kill-without-query process)))

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
      (with-temp-buffer
        (kawa-mode)
        (kawa-start)
        (let ((process (find-kawa-process)))
          (expect (find-kawa-process))
          (expect (process-live-p process)))))))
