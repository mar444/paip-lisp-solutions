(load "auxfns.lisp")

(defun new-cross-product (x y)
    (mappend #'(lambda (y-item)
        (mappend #'(lambda (x-item)
                   (list (list x-item y-item))) x)) y))

(print (new-cross-product '(1 2 3) '(4 5 6)))