(in-package :cl-user)

(defpackage :pcl.binary-data
  (:use :common-lisp)
  (:export :define-binary-class
	   :define-tagged-binary-class
	   :define-binary-type
	   :read-value
	   :write-value
	   :*in-progress-objects*
	   :parent-of-type
	   :current-binary-object
	   :+null+))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))


(defun as-keyword (sym) (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type, stream ,@args))))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun mklist (x) (if (listp x) x (list x)))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defmacro define-binary-class (name slots)
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ()
	 ,(mapcar #'slot->defclass-slot slots))
      
       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
	 (let ((,objectvar (make-instance ',name)))
	   (with-slots ,(mapcar #'first slots) ,objectvar
	     ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
	   ,objectvar))

       (defmethod write-value ((,typevar (eql ',name)) ,streamvar ,objectvar &key)
	 (with-slots ,(mapcar #'first slots) ,objectvar
	   ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots)))))))))























