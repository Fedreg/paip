;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straightforward implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sentence    () (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article     () (one-of '(the a)))
(defun Noun        () (one-of '(man ball woman table)))
(defun Verb        () (one-of '(hit took saw liked)))
(defun PP          () (append (Prep) (noun-phrase)))
(defun Adj         () (one-of '(big little blue green adiabatic fat slippery noisy lumpy)))
(defun Prep        () (one-of '(to in by with on)))

(defun one-of (set)
  "Pick one element of set and make a list of it"
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random"
  (elt choices (random (length choices))))

(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*)) nil))

;; (sentence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Based Implementation 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *simple-grammar*
  '((sentence    -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article     -> the a)
    (Noun        -> man ball woman table)
    (Verb        -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is *simple-grammar*, 
   but we can switch to other grammars.")

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defparameter *bigger-grammar*
  '((sentence    -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP* ) )
    (PP*         -> () (PP PP*))
    (Adj*        -> () (Adj Adj*))
    (PP          -> (Prep noun-phrase))
    (Prep        -> to in by with on)
    (Adj         -> big little blue green adiabatic)
    (Article     -> the a)
    (Name        -> Pat Kim Lee Terry Robin)
    (Noun        -> man ball woman table)
    (Verb        -> hit took saw liked)
    (Pronoun     -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

;; (generate 'sentence)
(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

;; (generate-tree 'Sentence)
