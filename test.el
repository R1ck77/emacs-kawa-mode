(require 'kawa-mode)

(defconst expected "#|kawa:1|# \(define x 12\)
|kawa:2|#\n")

(defun test-me ()
  "sends the expression into the REPL"
  (with-temp-buffer        
    (insert "\(define x 12\)\n")
    (kawa-mode)
    (kawa-eval-expr-at-point)
    (with-current-buffer (get-buffer "*Kawa REPL*")
      (sit-for 2)
      (buffer-substring-no-properties (point-min) (point-max)))))
