(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'kawa-mode-utils)

(describe "kawa-mode-utils.el"
  (describe "comment"
    (it "prevents the evaluation of the forms inside the comment"
      (spy-on 'message)
      (let ((x 12))
        (comment
         (setq x 44)
         (message "silenced message"))
        (expect x :to-be 12)
        (expect 'spy-on :not :to-have-been-called)))
    (it "can be nested"
      (spy-on 'message)
      (let ((x 12))
        (comment
         (setq x 44)
         (message "silenced message")
         (comment
          (setq x 45)
          (message "other silenced message")))
        (expect x :to-be 12)
        (expect 'spy-on :not :to-have-been-called))))
  (describe "when-let"
    (it "executes all bindings"
      (spy-on 'identity :and-call-through)
      (when-let ((var1 (identity 12))
                 (var2 (identity 13))
                 (var3 (identity nil)))
                55)
      (expect 'identity :to-have-been-called-times 3)
      (when-let ((var1 (identity 12))
                 (var2 (identity 13))
                 (var3 (identity t)))
                var3)
      (expect 'identity :to-have-been-called-times 6))
    (it "does not executes the body if the last assignment is nil"
      (spy-on 'message)
      (when-let ((var1 12)
                 (var2 13)
                 (var3 nil))
                (message "unexpected message")
                (message "unexpected message")                        
                50)
      (expect 'message :not :to-have-been-called))
    (it "executes the body if the last assignment is not nil"
      (spy-on 'message)
      (when-let ((var1 12)
                 (var2 13)
                 (var3 23))
                (message "first message")
                (message "second message")                        
                50)
      (expect (spy-calls-all-args 'message)
              :to-equal '(("first message") ("second message"))))
    (it "returns nil if the last assignment is nil"
      (spy-on 'message)
      (expect (when-let ((var1 12)
                  (var2 13)
                  (var3 23))
                 (message "first message")
                 (message "second message")                        
                 50)
              :to-be 50))
    (it "can reference the bindings in the body"
      (spy-on 'message)
      (expect (when-let ((var1 12)
                  (var2 13)
                  (var3 23))
                        (setq var1 0)
                        (+ var1 var2 var3))
              :to-be 36))))
