(load "auxfns.lisp")

(defstruct op "Operation"
    (action nil) (preconds nil) (add-list nil) (del-list nil))


(defun GPS (start-state goals ops)
  (let ((after-state-list (mappend #'(lambda (state) (apply-op state ops)) (list start-state))))
    (if (equal-sets after-state-list (list start-state))
        (some #'(lambda (state) (every #'(lambda (goal) (member goal state)) goals)) after-state-list)
        (some #'(lambda (state) (GPS state goals ops)) after-state-list))))

(defun equal-sets (x y)
  (every #'(lambda (item) (member item y)) x))

(defun apply-op (state ops)
  (cond ((null ops) (list state))
        
        ((every #'(lambda (precond) (member precond state)) (op-preconds (first ops)))
         (let ((op (first ops)))
           
         (append (list (append (op-add-list op) (set-difference state (op-del-list op)))) 
                 (apply-op state (rest ops)))))
        (t (apply-op state (rest ops)))))

(print (GPS '(a e) '(c e) (list (make-op :preconds '(a) :add-list '(c) :del-list '(a)))))
    