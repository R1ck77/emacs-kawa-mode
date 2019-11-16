
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

(provide 'kawa-mode-utils)
