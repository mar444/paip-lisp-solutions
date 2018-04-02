(load "auxfns.lisp")

(defun permutations (lst)
  (cond ((null lst) '(()))
        (t (mapcan #'(lambda (item)
                      (mapcar #'(lambda (permutation)
                                  (cons item permutation))
                               (permutations (remove item lst))))
                   lst))))


(print (permutations '(1 2 3 4)))