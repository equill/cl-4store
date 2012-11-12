(in-package #:4store)

(defun remove-if-typep (type list)
  (remove-if #'(lambda (x) (typep x type)) list))

;; From mon-key:
;;
;; Workspace.lisp originally set the symbol-function of `extract-data' and
;; `extract-rows' to `cl:sixth' and `cl:cdddr' respectively, and indicated with
;; the comment that these were: "More meaningful names"...
;; However, while this may be true, setf of symbol-function isn't my style :) 
;; Maybe these becomes a defgeneric at some point :)
;;
;; (setf (symbol-function 'extract-data) #'sixth)
;; (setf (symbol-function 'extract-rows) #'cdddr)

(defun row-extract (row-container)
  (cdddr row-container))

(defun data-extract (data-container)
  (sixth data-container))


;;; ==============================
;;; :DOCUMENTATION-FUN
;;; ==============================

;;; :SOURCE mcclim/Apps/Scigraph/dwim/extensions.lisp
;;; Which noted: "A somewhat consful implementation, but entirely portable."
(defun type-specifier-p (object)
  (let ((test #'(lambda (x) (typep 't x))))
    (when (or (symbolp object) (listp object))
      (multiple-value-bind (v errorp) (ignore-errors (funcall test object))
	(declare (ignore v))
	(not errorp)))))

(defun doc-set (name object-type string args)
  (declare (type symbol name) 
           ((member variable type function) object-type)
           ((or null string) string))
  (let ((doc-or-null 
         (if (null string)
             string
             (apply #'format nil `(,string ,@args)))))
        (ecase object-type
          (function
           (setf (documentation (fdefinition name) object-type) 
                 (setf (documentation name object-type) doc-or-null)))
      (variable 
       (locally (declare (special name))
         (setf (documentation name object-type) doc-or-null)))
      (type 
       (setf (documentation name object-type) doc-or-null)))))

(defun fundoc (name &optional string &rest args)
  (declare (type symbol name) ((or null string) string))
  (doc-set name 'function string args))

(defun vardoc (name &optional string &rest args)
  (declare (type symbol name)
           (special name) 
           ((or null string) string))
  (doc-set name 'variable string args))

(defun typedoc (name &optional string &rest args)
  (declare (type symbol name) 
           ((or null string) string))
  (when (type-specifier-p name)
    (doc-set name 'type string args)))

;;; ==============================
;;; EOF
