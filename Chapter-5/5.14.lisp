(defun my-mappend (fn &rest args)
  (apply #'append (apply #'mapcar fn args)))


(print (my-mappend #'(lambda (n1 n2) (list n1 n2)) '(1 2) '(3 4)))