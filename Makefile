.PHONY: test run

test:
	emacs --eval "(setq load-path (cons \".\" load-path))" -batch -f package-initialize -f buttercup-run-discover

run:
	emacs --eval "(setq load-path (cons \".\" load-path))" -nw -l kawa-mode.el test.scm

