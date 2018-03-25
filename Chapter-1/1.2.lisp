(defun power (a b)
    (if (= b 0) 1
        (* a (power a (- b 1)))))

(defun test ()
    (print (power 3 2)))

(test)
