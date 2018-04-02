(load "auxfns.lisp")

(defun permutations (lst)
  (cond ((null lst) nil)
        ((null (rest lst)) (list lst))
        (t  
    (mappend #'(lambda (item)
        (mappend #'(lambda (permutation)
            (list (append (list item) permutation)))
        (permutations (remove item lst))))
     lst))))


(print (permutations '(1 2 3 4)))