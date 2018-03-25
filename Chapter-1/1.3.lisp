(defun count-atoms (list)
    (cond ((null list) 0)
          ((atom (first list))
           (+ 1 (count-atoms (rest list))))
          (t
           (+ (count-atoms (first list)) (count-atoms (rest list))))))

(defun test ()
    (print (count-atoms '(a (nil b) c))))

(test)
