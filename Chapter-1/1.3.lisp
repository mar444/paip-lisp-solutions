(defun count-atoms (list)
    (if (null list) 0
        (if (atom (first list))
        (+ 1 (count-atoms (rest list)))
        (+ (count-atoms (first list)) (count-atoms (rest list))))))

(defun test ()
    (print (count-atoms '(a (nil b) c))))

(test)
