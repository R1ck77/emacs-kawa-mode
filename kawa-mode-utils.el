(require 'cl)

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
              (sleep-for check-interval)
              (if (> (- (time) start) timeout)
                  (error "Timeout waiting for the operation to finish!")
                (setq condition (funcall predicate-f)))))))

(defmacro defun- (name arguments &rest forms)
  `(defun ,name ,arguments
     (progn
       (message "\n*** %s called with %s\n" (symbol-name (quote ,name)) (list ,@arguments))
       ,@forms)))

(defun kawa--create-property-predicate (property &optional value)
  (lexical-let ((property property)
                (value value))
    (lambda (position)
      (let ((current-property (get-text-property position property)))
       (and current-property
            (or (not value) (equal value current-property)))))))

(defun kawa--search-backward (property-predicate)
  "Returns the first character that satisfies the predicate backwards (current position included)

It also doesn't save the excursion, but moves to the first
position that satisfies the property or to (point-min) if none can be found"
  (let ((result))
    (while (and (not result) (/= (point-min) (point)))
      (if (funcall property-predicate (point))
          (setq result (point))
        (goto-char (- (point) 1))))
    result))

(defun kawa--search-forward (property-predicate)
  "Returns the first character that satisfies the predicate forwards (current position included)

It also doesn't save the excursion, but moves to the first
position that satisfies the property or to (point-max) if none can be found"
  (let ((result))
    (while (and (not result) (/= (point-max) (point)))
      (if (funcall property-predicate (point))
          (setq result (point))
        (goto-char (+ (point) 1))))
    result))

(defun kawa-complement (f)
  (lexical-let ((f f))
    (lambda (&rest arguments)
      (not (apply f arguments)))))

(defun kawa-previous-bounds-with-property (property &optional value)
  "Return a cons cell with the start and end of a text with the specific properties before the point

nil is returned if no such text can be found"
  (save-excursion
    (let* ((predicate (kawa--create-property-predicate property value))
           (end (kawa--search-backward predicate)))
      (if end
          (let ((start (kawa--search-backward (kawa-complement predicate))))
            (cons (or (+ 1 start) (point-min)) (+ end 1)))))))

(defun kawa-next-bounds-with-property (property &optional value)
  "Return a cons cell with the start and end of a text with the specific properties before the point

nil is returned if no such text can be found"
  (save-excursion
    (let* ((current (point))
           (predicate (kawa--create-property-predicate property value))
           (start (kawa--search-forward predicate)))
      (if start
          (let ((end (kawa--search-forward (kawa-complement predicate))))
            (if end
                (cons start end)))))))

(defun kawa--get-history-exp-at-point ()
  (kawa--get-expression-matching-predicate-at-point (kawa--create-property-predicate 'kawa-history-expression)))

(defun kawa--get-expression-matching-predicate-at-point (predicate-f)
  (let ((current (point))
        (complemented (kawa-complement predicate-f))
        (start)
        (end))
    (setq start (kawa--search-backward complemented))
    (goto-char current)
    (setq end (kawa--search-forward complemented))
    (buffer-substring-no-properties (or (+ start 1) (point-min)) (or end (point-max)))))

(defun kawa--inside-history-expr-p (point)
  (get-text-property point 'kawa-history-expression))

(provide 'kawa-mode-utils)

