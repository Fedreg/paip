
(defstruct name
  first
  (middle nil)
  last)

(setf fed (make-name :first 'Fed :last 'Reggiardo))
(setf (name-middle fed) 'Nope)
(name-first fed)

(defun typer (x)
  (typecase x
    (number (abs x))
    (list (length x))))

(typer '(/ 1 9))

(member 2 '(1 2 3))

(defun length9 (list)
  (if (null list)
      0
      (+ 1 (length9 (rest list)))))

(length9 '(1 2 3 4 5 6 7 8 9))

(defmacro while (test &rest body)
  "Repeat body while test is true."
  `(loop (unless ,test (return nil))
      ,@body))

(setf i 1)

(while (< i 10)
  (print (* i i))
  (setf i (+ i 1)))

(princ (cons 2 3))
(equal "dog" "Dog")
(equalp "dog" "Dog")
(equalp 1 2)

(defun adder (n)
  (+ 2 (* 2 (/ 2 (+ 2 (- 2 n))))))

(adder 45)

(defun dotted-pair (exp)
  (list* 
   (princ (first exp))
   (princ (first (rest exp)))))
  
(dotted-pair '(1 2))

(elt '(1 2 3) 0)

(setq state-table
      '((AL . Alabama) (AK . Arkansas) (AZ . Arizona)))

(assoc 'AK state-table)

(setf table (make-hash-table))

(setf (gethash 'AL table) 'Alabama)
(setf (gethash 'AK table) 'Alaska)
      
(print table)

(mod 11 5)

(format t "~a plus ~s is ~f" "two" "two" 4)
(format t "~&~@r" 1579)
