(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'kawa-mode)

(describe "kawa-mode.el"
  (describe "kawa-mode"
    (it "sets the current buffer's mode and name"
      (with-temp-buffer
        (kawa-mode)
        (expect mode-name
                :to-equal "Kawa")
        (expect major-mode
                :to-equal 'kawa-mode)))))
