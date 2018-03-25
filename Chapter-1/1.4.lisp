(defun count-anywhere (target exp)
    (cond ((eq target exp) 1)
          ((atom exp) 0)
          (t (+ (count-anywhere target (first exp)) 
                (count-anywhere target (rest exp))))))

(defun test ()
    (print (count-anywhere  'a '(a (nil a a b) c))))

(test)