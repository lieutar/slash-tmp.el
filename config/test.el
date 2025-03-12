;; -*- lexical-binding: t -*-
(require 'ert)

(unless command-line-args-left
  (error (concat "Usage: emacs --batch -Q -l your-test-runner.el"
                 " test-file1.el test-file2.el ...")))

(dolist (file command-line-args-left)
  (load (expand-file-name file)))
(ert-run-tests-batch)

(message "All tests completed.")
