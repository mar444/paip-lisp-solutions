(defun dot-product (list-1 list-2)
    (apply #'+ (mapcar #'* list-1 list-2)))

(defun test ()
    (print (dot-product '(10 20) '(3 4))))

(test)