(load "auxfns.lisp")

(defun new-cross-product (fn xlist ylist)
    (mappend #'(lambda (yitem)
        (mapcar #'(lambda (xitem)
                   (funcall fn xitem yitem)) xlist)) ylist))

(defun new-combine-all (xlist ylist)
    (new-cross-product #'append xlist ylist))

(print (new-combine-all '((1 2) (2 3)) '((4 5) (5 6))))