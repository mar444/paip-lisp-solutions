(load "auxfns.lisp")

(defstruct op "Operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defvar *visited* nil)

(defun GPS (start-state goals ops)    
  (cond ((achieve-all start-state goals)
         start-state)
        ((member start-state *visited*) nil)
        (t (progn (setf *visited* (union *visited* (list start-state)))
                 (let ((next-state-list (apply-op start-state ops)))
                   (print next-state-list)
                   (if (null next-state-list) nil
                       (or (some #'(lambda (state) (achieve-all state goals)) next-state-list)
                           (some #'(lambda (state) (GPS state goals ops)) next-state-list))))))))


(defun achieve-all (state goals)
  (every #'(lambda (goal) (member goal state)) goals))

(defun equal-sets (x y)
  (every #'(lambda (item) (member item y)) x))

;; return a list of state that results from applying operation on the given state
(defun apply-op (state ops)
  (cond ((null ops) nil)
        
        ((every #'(lambda (precond) (member precond state)) (op-preconds (first ops)))
         
         (let* ((op (first ops))
                (next-state (union (op-add-list op) (set-difference state (op-del-list op)))))
           (if (not (member next-state *visited*))
               (progn (setf *visited* (union *visited* (list next-state)))
                      ; (print next-state)
                      (append (list next-state)
                              (apply-op state (rest ops))))
               (apply-op state (rest ops)))))
        
        (t (apply-op state (rest ops)))))

(defparameter *school-ops*
  (list
    (make-op :action 'drive-son-to-school
             :preconds '(son-at-home car-works)
             :add-list '(son-at-school)
             :del-list '(son-at-home))
    (make-op :action 'shop-installs-battery
             :preconds '(car-needs-battery shop-knows-problem shop-has-money)
             :add-list '(car-works))
    (make-op :action 'tell-shop-problem
             :preconds '(in-communication-with-shop)
             :add-list '(shop-knows-problem))
    (make-op :action 'telephone-shop
             :preconds '(know-phone-number)
             :add-list '(in-communication-with-shop))
    (make-op :action 'look-up-number
             :preconds '(have-phone-book)
             :add-list '(know-phone-number))
    (make-op :action 'give-shop-money
             :preconds '(have-money)
             :add-list '(shop-has-money)
             :del-list '(have-money))))

(defvar *ops* *school-ops*)

(print (GPS '(a b f) '(d e) 
            (list (make-op :action 't1
                           :preconds '(a)
                           :add-list '(d))
                  (make-op :action 't2
                           :preconds '(b)
                           :add-list '(f))
                  (make-op :action 't3
                           :preconds '(f)
                           :add-list '(e)))))

; (print (GPS '(SON-AT-HOME CAR-NEEDS-BATTERY HAVE-money have-phone-book) '(son-at-school) *ops*))

