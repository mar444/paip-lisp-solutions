(load "auxfns.lisp")
(load "tests.lisp")
(load "ops.lisp")

(defvar *state* nil "The current state: a list of conditions.")

(defvar *ops* nil "A list of available operators.")

(defun GPS (*state* goals *ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (achieve-all goals) 'solved))

(defun achieve (goal)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (or (member goal *state*)
      (some #'new-apply-op 
            (new-find-all goal *ops* :test #'new-appropriate-p))))

(defun new-find-all (item sequence &key (test #'eql))
  (remove-if-not #'(lambda (x) (apply test (list item x))) sequence))

(defun new-appropriate-p (goal op)
  (member goal (op-add-list op)))

(defun new-apply-op (op)
  (if (achieve-all (op-preconds op))
      (progn 
        (print (list 'executing (op-action op)))
        (setf *state* (set-difference (union *state* (op-add-list op)) (op-del-list op))))
      nil))

(defun achieve-all (goals)
  (every #'achieve goals))

(run-school-tests #'GPS *ops*)
