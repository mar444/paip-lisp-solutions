(load "auxfns.lisp")

(defstruct op "Operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defvar *visited* nil)

(defun GPS (start-state-actions goals ops)
  (let ((start-state (first start-state-actions))
        (start-actions (second start-state-actions)))
    (cond ((visited-p start-state) nil)
        ((achieve-all start-state goals) start-actions)
        (t (progn (setf *visited* (union *visited* (list start-state)))
                  (let ((next-state-list (apply-op start-state start-actions ops)))
                    (if (null next-state-list) nil
                        (some #'(lambda (state) (GPS state goals ops)) next-state-list))))))))   
  

(defun visited-p (state)
  (some #'(lambda (visited-state) (equal-sets visited-state state)) *visited*))

(defun achieve-all (state goals)
  (every #'(lambda (goal) (member goal state)) goals))

(defun equal-sets (x y)
  (and (every #'(lambda (item) (member item y)) x)
       (every #'(lambda (item) (member item x)) y)))

;; return a list of state that results from applying operation on the given state
(defun apply-op (state actions ops)
  (cond ((null ops) nil)
        
        ((every #'(lambda (precond) (member precond state)) (op-preconds (first ops)))
         
         (let* ((op (first ops))
                (next-state (union (op-add-list op) (set-difference state (op-del-list op))))
                (next-actions (append actions (list (op-action op)))))
           (append (list (list next-state next-actions))
                   (apply-op state actions (rest ops)))))
        
        (t (apply-op state actions (rest ops)))))

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

;; 4.4 TEST CASES

;; SOLVED 
(print (GPS '((SON-AT-HOME CAR-NEEDS-BATTERY HAVE-MONEY HAVE-PHONE-BOOK)) '(SON-AT-SCHOOL) *ops*))

;; NIL
(print (GPS '((SON-AT-HOME CAR-NEEDS-BATTERY HAVE-MONEY)) '(SON-AT-SCHOOL) *ops*))

;; SOLVED
(print (GPS '((SON-AT-HOME CAR-WORKS)) '(SON-AT-SCHOOL) *ops*))


;; 4.7 TEST CASES 

;; SOLVED
(print (GPS '((SON-AT-HOME HAVE-MONEY CAR-WORKS)) '(HAVE-MONEY SON-AT-SCHOOL) *ops*))

;; NIL
(print (GPS '((SON-AT-HOME CAR-NEEDS-BATTERY HAVE-MONEY HAVE-PHONE-BOOK)) '(SON-AT-SCHOOL HAVE-MONEY) *ops*))