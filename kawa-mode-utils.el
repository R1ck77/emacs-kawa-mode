
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

(defun kawa--wait-condition-with-timeout (predicate-f timeout &optional check-interval)
  (let ((check-interval (or check-interval 0.1))
        (start (time)))
    (while (funcall predicate-f)
      (sit-for check-interval)
      (if (> (- (time) start) timeout)
          (error "Timeout waiting for the operation to finish!")))))

(provide 'kawa-mode-utils)
