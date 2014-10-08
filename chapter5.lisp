; Chapter five

(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d~%" x y)
  (+ x y))

; Optional parameters

(defun foo (a b &optional c d) (list a b c d)) 

(defun foo (a &optional (b 10)) (list a b))

(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

(defun foo (&key a b c) (list a b c))

(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))

(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))