
(defmacro comment (&rest forms))

(defmacro when-let (definition &rest forms)
  (declare (indent 1))
  "Allows to write:

\(when-let \(\(var1 expr\)
         \(var2 expr\)\)
    internal forms\)

which will return nil without evaluating the internal forms, or the result of
the internal forms evaluation (where the bindings are visible) if var2 is true"
  `(let ,definition
     (when ,(car (car (reverse definition)))
       (progn ,@forms))))

(defun time ()
  (let ((time (current-time)))
    (+ (elt time 1) (/ (elt time 2) 1e6))))

(defun kawa--while-with-timeout (predicate-f timeout &optional check-interval)
  (let ((check-interval (or check-interval 0.01))
        (start (time)))
    (let ((condition (funcall predicate-f)))
      (while condition
              (sit-for check-interval)
              (if (> (- (time) start) timeout)
                  (error "Timeout waiting for the operation to finish!")
                (setq condition (funcall predicate-f)))))))

(defmacro defun- (name arguments &rest forms)
  `(defun ,name ,arguments
     (progn
       (message "\n*** %s called with %s\n" (symbol-name (quote ,name)) (list ,@arguments))
       ,@forms)))

(provide 'kawa-mode-utils)

