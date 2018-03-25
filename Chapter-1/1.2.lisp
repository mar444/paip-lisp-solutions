(defun power (a b)
    (if (= b 0) 1
        (* a (power a (- b 1)))))

(defun power-log (a b)
  "This executes in log n time"
  (cond ((= b 0) 1)
        ((evenp b) (expt (power-log a (/ b 2)) 2))
        (t (* a (power-log a (- b 1))))))

(defun test ()
    (print (power-log 3 2)))

(test)
