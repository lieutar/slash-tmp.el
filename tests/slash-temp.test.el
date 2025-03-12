(require 'ert)
(require 'slash-tmp)


(ert-deftest /tmp-test/--/simple-utils/ ()
  "Tests for simple utility functions"
  (let ((file (/tmp/make-temp-file))
        (dir  (/tmp/make-temp-dir)))
    (should (file-exists-p file))
    (should-not (file-directory-p file))
    (should (file-directory-p dir))
    (should (file-directory-p dir))
    (delete-file file)
    (delete-directory dir t)
    ))

(ert-deftest /tmp-test/--/simple-utils/with-args/ ()
  ""
  (let* ((file (/tmp/make-temp-file
                :prefix "hoge-"
                :suffix ".fuga"
                :content "hoge.fuga"))
         (base    (file-name-nondirectory file))
         (content (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-substring-no-properties (point-min)(point-max)))))
    (should (string-match "\\`hoge-.*?\\.fuga\\'" base))
    (should (equal content "hoge.fuga"))
    (delete-file file)))


(ert-deftest /tmp-test/--/tmp/let ()
  "Tess for /tmp/let"
  (let (aa bb cc dd ee ff gg)
    (/tmp/let (a b c/ d e/ f g)
      (setq aa a)
      (setq bb b)(setq cc c/)
      (setq dd d)(setq ee e/)
      (setq ff f)
      (setq gg g)
      (dolist (file (list a b d f g))
        (should (stringp file))
        (should (file-exists-p file))
        (should-not (file-directory-p file)))
      (dolist (dir (list c/ e/))
        (should (stringp dir))
        (should (file-exists-p dir))
        (should (file-directory-p dir))))
    (dolist (file (list aa bb cc dd ee ff gg))
      (should-not (file-exists-p file)))
    )
  )


(ert-deftest /tmp-test/--/tmp/with-temp-dir ()
  "Tests for /tmp/with-temp-dir"
  (let ((cwd default-directory)
        (dir nil))
    (/tmp/with-temp-dir nil
      (setq dir default-directory)
      (should-not (equal cwd dir))
      (should (file-exists-p dir))
      (should (file-directory-p dir)))
    (should-not (file-exists-p dir))))
