
(defparameter *titles*
  '(Mr Mrs Ms Miss Dr Sir Madam Major General Jr MD))

(defun last-name (name)
  "Retruns the last name in a list without any titles"
  (if (member (first (last name)) *titles*)
      (last-name (reverse (rest (reverse name))))
      (first (last name))))
 
;(last-name '(Rex Morgan Jr))

(defun power (n exp)
  "Raises a number to an integer power"
  (apply #'* (loop for x from 1
                while (<= x exp)
                collect n)))

;; (power 3 3)

(defun count-atoms (exp)
  "Return the total number of non-nil atoms in the expression."
  (cond ((null exp) 0)
        ((atom exp) 1)
        (t (+ (count-atoms (first exp))
              (count-atoms (rest exp))))))

;; (count-atoms '(1 (nil) 4))

(defun count-anywhere (item tree)
  "Count the times item appears anywhere within tree."
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere item (first tree))
              (count-anywhere item (rest tree))))))

;; (count-anywhere 'a '(a ('a) 'a))

(defun dot-product (a b)
  "The dot product is computed by multiplying corresponding elements and then adding up the resulting products."
  (apply #'+ (mapcar #'* a b)))

;; (dot-product '(10 20) '(3 4))
