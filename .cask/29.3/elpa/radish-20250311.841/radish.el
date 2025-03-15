;; -*- lexical-binding: t -*-
;;; radish.el --- yet another DI Container for Emacs lisp

;; Copyright (C) 2025  lieutar <lieutar@gmail.com>

;; Author: lieutar
;; Version: 0.0.0
;; Package-Version: 20250311.841
;; Package-Commit: 5b5d515725ee953366280f4e9986adbae0f9606a
;; Keywords: Development, Programming, Elisp
;; URL:

;;; License:

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Radish in Japanese is "daikon", and some Japanese developers say "daikon"
;; about DI Containers (derived from DI-Con).
;;
;; This is not an ordinary implementation of DI Container, it provides
;; injection of dependency about only functions.
;; But I think it is enough in most cases.

;;; Code:
(provide 'radish)

;;;; internal

(defconst radish::$container-documents-alist ())

(defsubst radish::container-alist-name (container)
  (intern (format "-*-radish-container<%s>-alist-*-" container)))

(defsubst radish::container-alist (container)
  (let ((sym (radish::container-alist-name container)))
    (unless (boundp sym)
      (error "undefined container: %s" container))
    (symbol-value sym)))

(defun radish::init-container-alist (container funcs)
  (set (radish::container-alist-name container)
       (mapcar (lambda (it) (if (symbolp it)(cons it it)it)) funcs)))

(defun radish::get-func (container name)
  (let ((alist (radish::container-alist container)))
    (unless alist (error (format "undefined container: %s" container)))
    (let ((func (cdr (assq name alist))))
      (unless func (error (format "container `%s' doesn't have a function `%s'"
                                  container name)))
      func)))

(defsubst radish::make-func (container func)
  (unless (fboundp func) (error (format "undefined function:%s" func)))
  (let ((name         (intern (format "%s@%s" func container)))
        (docstr       (format (concat "Wrapper of the function `%s'"
                                        " for the `radish' container `%s'")
                                func container))
          (arg-spec     (help-function-arglist func))
          (requires-apply nil)
          (r-args-to-call nil))
      (dolist (arg arg-spec)
        (cond ((eq arg '&optional))
              ((eq arg '&rest) (setq requires-apply t))
              (t (push arg r-args-to-call))))
      (let ((args-to-call (reverse r-args-to-call)))
        (defalias name
          (byte-compile
           `(lambda ,arg-spec
              ,docstr
              (apply (radish::get-func ',container ',func)
                     ,@(append args-to-call
                               (unless requires-apply (list nil))))))))))

(defun radish::make-funcs (container funcs)
  (dolist (func funcs)
    (radish::make-func container  func)))

(defun radish::make-container (name docstr funcs)
  (let ((slot (assq name radish::$container-documents-alist)))
    (if slot (setcdr slot docstr)
      (push (cons name docstr) radish::$container-documents-alist)))
  (radish::make-funcs name funcs)
  (radish::init-container-alist name funcs)
  name)

;;;; exports

(defun radish-container-p (name)
  "Returns non nil value, when the container having specified NAME was defined."
  (and (assq name radish::$container-documents-alist) t))

(defun radish-documentation (name)
  "Returns documentation of the container having the NAME."
  (cdr (assq name radish::$container-documents-alist)))

(defmacro radish-define-container (name &rest body)
  "Creates new container for your project."
  (declare (indent defun))
  (let ((head (car body))
        (docstr nil))
    (when (stringp head)
      (setq docstr head)
      (setq body (cdr body)))
    `(radish::make-container ',name ,docstr ',body)))

(defmacro with-radish-container (name mock-list &rest body)
  "Runs BODY with temporary binding with MOCK-LIST to
 the container having the NAME."
  (declare (indent defun))
  `(let* ((original-alist (radish::container-alist ',name))
          (temp-alist     (copy-alist original-alist)))
     (dolist (mock-spec ',mock-list)
       (let* ((func-name (car mock-spec))
              (temp-slot (assq func-name temp-alist)))
         (unless temp-slot
           (error "Undefined slot for \"%s\"" func-name))
         (setcdr temp-slot (eval (cadr mock-spec)))))
     (set (radish::container-alist-name ',name) temp-alist)
     (unwind-protect (progn ,@body)
       (set (radish::container-alist-name ',name) original-alist))))

;;; radish.el ends here
