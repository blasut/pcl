(defpackage :pcl.html
  (:use :common-lisp :pcl.macro-utilities)
  (:export :with-html-output
           :in-html-style
           :define-html-macro
           :html
           :emit-html
           :&attributes))

(defparameter *element-escapes* "<>&")
(defparameter *attribute-escapes* "<>&\"'")
(defparameter *escapes* *element-escapes*)

(defun self-evaluating-p (form)
  (and (atom form) (if (symbolp form) (keywordp form) t)))


(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (or (funcall test (car form))
	   (and (consp (car form)) (funcall test (caar form))))))

(defun parse-explicit-attributes-sexp (sexp)
  (destructuring-bind ((tag &rest attributes) &body body) sexp
    (values tag attributes body)))

(defun parse-implicit-attributes-sexp (sexp)
  (loop :with tag = (first sexp)
     :for rest :on (rest sexp) by #'cddr
     :while (and (keywordp (first rest)) (second rest))
     :when (second rest)
        :collect (first rest) :into attributes :and
        :collect (second rest) :into attributes
     :end
     :finally (return (values tag attributes rest))))

(defun parse-cons-form (sexp)
  (if (consp (first sexp))
      (parse-explicit-attributes-sexp sexp)
      (parse-implicit-attributes-sexp sexp)))


(defun escape-char (char)
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))


(defun escape (in to-escape)
  (flet ((needs-escape-p (char) (find char to-escape)))
    (with-output-to-string (out)
      (loop :for start = 0 :then (1+ pos)
	    :for pos = (position-if #'needs-escape-p in :start start)
	    :do (write-sequence in out :start start :end pos)
	    :when pos :do (write-sequence (escape-char (char in pos)) out)
	    :while pos))))

(defclass indenting-printer ()
  ((out                 :accessor out                 :initarg out)
   (beginning-of-line-p :accessor beginning-of-line-p :initform t)
   (indention           :accessor indention           :initform 0)
   (indenting-p         :accessor indenting-p         :initform t)))

(defun indent-if-necessary (ip)
  (when (and (beginning-of-line-p ip) (indenting-p ip))
    (loop repeat (indention ip) do (write-char #\Space (out ip)))
    (setf (beginning-of-line-p ip) nil)))

(defun emit/no-newlines (ip string &key (start 0) end)
  (indent-if-necessary ip)
  (write-sequence string (out ip) :start start :end end)
  (unless (zerop (- (or end (length string)) start))
    (setf (beginning-of-line-p ip) nil)))

(defun emit-newline (ip)
  (write-char #\Newline (out ip))
  (setf (beginning-of-line-p ip) t))

(defun emit-freshline (ip)
  (unless (beginning-of-line-p ip) (emit-newline ip)))

(defun emit (ip string)
  (loop :for start = 0 then (1+ pos)
     :for pos = (position #\Newline string :start start)
     :do (emit/no-newlines ip string :start start :end pos)
     :when pos :do (emit-newline ip)
     :while pos))


;;;;; Backend interace

(defgeneric raw-string (processor string &optional newlines-p))

(defgeneric newline (processor))

(defgeneric freshline (processor))

(defgeneric indent (processor))

(defgeneric unindent (processor))

(defgeneric toggle-indenting (processor))

(defgeneric embed-value (processor value))

(defgeneric embed-code (processor code))

;;;;; Backend implementation




















