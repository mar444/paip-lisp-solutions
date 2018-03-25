(defun count-atoms (list)
    (cond ((null list) 0)
          ((atom list) 1)
          (t (+ (count-atoms (first list)) 
                (count-atoms (rest list))))))

(defun test ()
    (print (count-atoms '(a (b) c))))

(test)