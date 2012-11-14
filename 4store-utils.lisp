(in-package #:4store)

(defun angle-bracket-delimited-p (str)
  "Detects whether a string is delimited by angle-brackets."
  (cl-ppcre:scan "^<[:\\w]+>$" str))

(defun quote-plaintext (str)
  "Wraps the string in quotes if it isn't delimited by angle-brackets.
Intended for the object of a triple on insertion."
  (if (angle-bracket-delimited-p str)
      str
    (cl-ppcre:regex-replace
     "$"
     (cl-ppcre:regex-replace "^" str "\"")
     "\"")))

(defun render-parsed-uri-to-string-if (uri-or-string)
  (if (puri:uri-p uri-or-string)
      (with-output-to-string (as-rendered)
        (puri:render-uri uri-or-string as-rendered))
      uri-or-string))

(defun map-uris-to-strings (list)
  (mapcar #'render-parsed-uri-to-string-if list))

(defun render-request-as-xmls (xml-result)
  (cxml:parse xml-result (cxml-xmls:make-xmls-builder)))

(defun foaf-person-construct-extract ()
  (let ((extracted-persons '()))
    (flet ((render-parsed-persons (s p o)
             (push (map-uris-to-strings (list s p o))
                   extracted-persons)))
      (cl-rdfxml:parse-document #'render-parsed-persons  (foaf-person-construct-request)))
    ;; `cl:nreverse' keeps same order as RDF/XML
    (setf extracted-persons (nreverse extracted-persons))))

(defun rdfs-class-select-render-as-xmls (request-data)
  (render-request-as-xmls request-data))

(defun rdfs-class-select-row-data-extract (rendered-xmls)
  (row-extract (data-extract rendered-xmls)))

(defun rdfs-class-select-string-filter (extracted-row-data)
  (remove-if-typep 'string extracted-row-data))

(defun rdfs-class-uri-match (item)
  ;; Underscores in pattern are "don't care" positions.
  ;; The 'uri' variable is the position of interest.
  (fare-matcher:letm (list _ _ _ (list _ _  (list _ _ uri)) _) 
                     item 
                     uri))

(defun rdfs-class-uri-match-map (filtered-data)
  (mapcar #'rdfs-class-uri-match filtered-data))

(defun rdfs-class-select-extract ()
  (rdfs-class-uri-match-map
   (rdfs-class-select-string-filter    
    (rdfs-class-select-row-data-extract
     (rdfs-class-select-render-as-xmls 
      (rdfs-class-select-request))))))

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
