; (load "auxfns.lisp")
(load "tests.lisp")

(defconstant fail nil)
(defconstant no-bindings '((t . t)))


(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun segment-p (x)
  (and (consp x)
       (starts-with x '?*)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((and (atom pattern) (atom input) (eql pattern input)) bindings)
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) bindings)))
        (t fail)))


(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal (binding-val binding) input) bindings)
          (t fail))))


(defun match-segment (pattern input result &optional (start 0))
  (let* ((var (second (first pattern)))
         (target (first (rest pattern)))
         (pos (find target input :start start :test #'equal)))
    (if (null pos)
        nil
        (let ((result-2 (pat-match (rest pattern) (subseq input pos))))
          (if (null result-2)
              (match-segment pattern input result (+ pos 1))
              (append (list (cons var (subseq input 0 pos))) result-2))))))


(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun extend-bindings (var val bindings)
  (cons (cons var val) 
        (if (eq bindings no-bindings)
            nil
            bindings)))


; (print (pat-match '((?* ?P) need (?* ?X)) '(I really need A F)))

; (print (pat-match '((?* ?P) need (?* ?X)) '(I really need A F)))

(run-tests #'pat-match)

