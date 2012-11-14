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
