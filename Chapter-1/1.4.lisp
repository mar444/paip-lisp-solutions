(defun count-anywhere (target exp)
    (cond ((null exp) 0)
          ((atom (first exp))
           (+ (if (eq target (first exp)) 1 0) (count-anywhere target (rest exp))))
          (t
           (+ (count-anywhere target (first exp)) (count-anywhere target (rest exp))))))

(defun test ()
    (print (count-anywhere  'a '(a (nil b) c))))

(test)