(load "auxfns.lisp")
(load "tests.lisp")
(load "ops.lisp")

(defvar *visited* nil)


(defun GPS (start-state-actions goals ops)
  (GPS-helper `(,start-state-actions) goals ops))

(defun GPS-helper (start-state-actions goals ops)
  (let ((start-state (first start-state-actions))
        (start-actions (second start-state-actions)))
    (cond ((visited-p start-state) nil)
        ((achieve-all start-state goals) start-actions)
        (t (progn (setf *visited* (union *visited* (list start-state)))
                  (let ((next-state-list (apply-op start-state start-actions ops)))
                    (if (null next-state-list) nil
                        (some #'(lambda (state) (GPS-helper state goals ops)) next-state-list))))))))   
  

(defun visited-p (state)
  (some #'(lambda (visited-state) (equal-sets visited-state state)) *visited*))

(defun achieve-all (state goals)
  (every #'(lambda (goal) (member-equal goal state)) goals))

(defun equal-sets (x y)
  (and (every #'(lambda (item) (member-equal item y)) x)
       (every #'(lambda (item) (member-equal item x)) y)))

(defun member-equal (item sequence)
  (member item sequence :test #'equal))

;; return a list of state that results from applying operation on the given state
(defun apply-op (state actions ops)
  (cond ((null ops) nil)
        
        ((every #'(lambda (precond) (member-equal precond state)) (op-preconds (first ops)))
         
         (let* ((op (first ops))
                (next-state (union (op-add-list op) (set-difference state (op-del-list op))))
                (next-actions (append actions (list (op-action op)))))
           (append (list (list next-state next-actions))
                   (apply-op state actions (rest ops)))))
        
        (t (apply-op state actions (rest ops)))))


; (run-school-tests #'GPS *school-ops*)

(run-blocks-tests #'GPS *blocks-ops*)