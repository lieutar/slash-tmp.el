;; -*- lexical-binding: t -*-
;;; slash-tmp.el --- Utilities for temporary files and directories.

;; Copyright (C) 2025  lieutar <lieutar@gmail.com>

;; Author: lieutar
;; Version: 0.0.0
;; Keywords: Programming, Elisp, Development
;; URL:

;;; License:

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(provide 'slash-tmp)
(require 'radish)

(radish-define-container /tmp/
  "DI Container for slash-tmp"
  make-temp-file
  delete-file
  delete-directory)



;;;; simple utils

(defsubst /tmp/make-temp-file--validate (spec)
  (let ((len (length spec))
        (n   0))
    (while (< n len)
      (let ((key (nth n spec)))
        (unless (keywordp key)
          (error "keywordp: %S" key))
        (unless (memq key '(:type :content :prefix :suffix))
          (error "unknown keyword found: %S" key)))
      (setq n (+ n 2))))
  (let ((type    (plist-get spec :type))
        (prefix  (plist-get spec :prefix))
        (suffix  (plist-get spec :suffix))
        (content (plist-get spec :content)))
    (when (and type (null (memq type '(file dir))))
      (error "unknown :type %s" type))
    (when (and prefix (not (stringp prefix)))
      (error ":prefix have to be a string: %S" prefix))
    (when (and suffix (not (stringp suffix)))
      (error ":suffix have to be a string: %S" suffix))
    (when content
      (unless (stringp content)
        (error ":content have to be a string: %S" content))
      (when (eq type 'dir)
        (error "directory cant have it's content")))))

;;;###autoload
(defun /tmp/make-temp-file (&rest spec)
  "Create temporary file."
  (/tmp/make-temp-file--validate spec)
  (let ((prefix    (or (plist-get spec :prefix)  "slash-tmp--file-"))
        (suffix    (or (plist-get spec :suffix)  nil))
        (dir-flag  (eq (plist-get spec :type)    'dir))
        (content   (or (plist-get spec :content) nil)))
    (let ((result (make-temp-file@/tmp/ prefix dir-flag suffix content)))
      (while (not (file-exists-p result)) (sit-for 0.005))
      result)))

;;;###autoload
(defun /tmp/make-temp-dir (&rest spec)
  "Create temporary directory."
  (if spec (plist-put spec :type 'dir) (setq spec (list :type 'dir)))
  (unless (plist-get spec :prefix) (plist-put spec :prefix "slash-tmp--fir-"))
  (apply #'/tmp/make-temp-file spec))


;;;; /tmp/let

(defsubst /tmp/let--validate-binding (binding)
  (unless (symbolp (car binding))
    (error "malformed binding list %S: symbolp: %S" binding (car binding)))
  (unless (plist-get (cdr binding) :type)
    (error ":type is required")))


(defun /tmp/let--prepare-binding (binding)
  (setq binding
        (if (symbolp binding)
            (list binding (if (string-suffix-p "/" (symbol-name binding))
                              ''dir ''file))
          binding))
  (let ((spec (cdr binding)))
    (if (= 1 (length spec))
        (let ((shorten-spec (car spec)))
          (cond ((stringp shorten-spec)
                 (setcdr binding (list :type 'file :content shorten-spec)))
                ((and (listp shorten-spec)
                      (eq 'quote (car shorten-spec)))
                 (setcdr binding (list :type shorten-spec)))))))
  (/tmp/let--validate-binding binding)
  binding)
;;(insert "\n" (pp(macroexpand '(/tmp/let ((a :type 'file :content "xyz" :prefix (concat "a-" "b")))))))


(defun /tmp/let--let-binding (binding)
  (let ((var  (car  binding))
        (spec (cdr  binding)))
    `(,var (apply #'/tmp/make-temp-file ,(cons 'list spec)))))

(defun /tmp/let--delete (file)
  (when (file-exists-p file)
    (if (file-directory-p file)
        (delete-directory@/tmp/ file t)
      (delete-file@/tmp/ file))))

;;;###autoload
(defmacro /tmp/let (bindings &rest body)
  "Create temporary files and directories for the duration of BODY.
BINDINGS is a list of variables to create temporary files or directories.

Each BINDINGS are symbols or lists.

"
  (declare (indent defun))
  (setq bindings (mapcar #'/tmp/let--prepare-binding bindings))
  `(let ,(mapcar #'/tmp/let--let-binding bindings)
     (unwind-protect
         (progn
           ,@body)
       (dolist (file (list ,@(mapcar #'car bindings)))
         (/tmp/let--delete file)))))


;;;;
;;;###autoload
(defmacro /tmp/with-temp-dir (definition &rest body)
  "Execute BODY with `default-directory' set to a temporary directory.
The temporary directory will be deleted after BODY is executed."
  (declare (indent defun))
  `(/tmp/let ((default-directory 'dir))
     ,@body))

;;(/tmp/with-temp-dir nil )

;;; slash-tmp.el ends here
