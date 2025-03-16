(require 'ert)
(require 'slash-tmp)


(ert-deftest /tmp-test/--/simple-utils/ ()
  "Tests for simple utility functions in simplest cases"
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
  "Tests for utility functions with details"
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

(ert-deftest /tmp-test/--/tmp/let/simplest-usage ()
  "Tess for /tmp/let to simplest case"
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


(ert-deftest /tmp-test/--/tmp/let/with-args ()
  ""
  (let ((common-prefix "tmp-test-")
        aa bb)
    (/tmp/let ((a :type 'dir
                  :prefix (concat common-prefix "a-")
                  :suffix "-dir")
               (b :type 'file
                  :prefix (concat common-prefix "b-" )
                  :suffix "-file"
                  :content "content-of-b"))
      (setq aa a)
      (setq bb b)
      (should (stringp a))
      (should (file-directory-p a))
      (should (string-match "[/\\\\]tmp-test-a-" a))
      (should (string-match "-dir\\'" a))
      (should (stringp b))
      (should-not (file-directory-p b))
      (should (string-match "[/\\\\]tmp-test-b-" b))
      (should (string-match "-file\\'" b))
      (should (equal "content-of-b"
                     (with-temp-buffer
                       (insert-file-contents b)
                       (buffer-substring-no-properties (point-min)(point-max)))))
      )
    (should-not (file-exists-p aa))
    (should-not (file-exists-p bb))))
;;(ert-run-test '/tmp-test/--/tmp/let/with-args)



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
