(load "auxfns.lisp")
(load "tests.lisp")
(load "rules.lisp")

(defvar *topics* '(you))

(defvar *equals* '(((everybody) everyone)))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun segment-p (x)
  (and (consp x)
       (starts-with (car x) '?*)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-p pattern)
         (match-segment pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) bindings)))
        (t fail)))


(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal (binding-val binding) input) bindings)
          (t fail))))


(defun match-segment (pattern input bindings &optional (start 0))
  (let* ((var (second (first pattern)))
         (pat (rest pattern)))
    (if (null pat) (match-variable var input bindings)
        (let ((pos (position (first pat) input :start start :test #'equal)))
          (if (null pos) fail
              (let ((result-2 (pat-match pat (subseq input pos)
                                         (match-variable var (subseq input 0 pos) bindings))))
                (if (eq result-2 fail)
                    (match-segment pattern input bindings (+ pos 1))
                    result-2)))))))


(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun extend-bindings (var val bindings)
  (cons (cons var val) 
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun starts-with (list x) (and (consp list) (eql (first list) x)))

(defun random-elt (seq) (elt seq (random (length seq))))


(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defun robot (rules)
  (loop
    (print 'robot>)
    (let* ((input (normalize (read-line-no-punct)))
           (result (flatten (use-rules input rules)))
           (response (if (null result)
                         `(Tell me more about ,(random-elt *topics*))
                         result)))
      (print-with-spaces response)
      (if (equal response '(good bye)) (RETURN)))))


(defun print-with-spaces (list)
  (format t "~{~a ~}" list))

(defun read-line-no-punct ()
  (read-from-string 
    (concatenate 'string "("
                 (substitute-if #\space #'(lambda (char) 
                                           (find char ".,;:`!?#-()\\\"")) (read-line))
                 ")")))


(defun extend-topics (topic)
  (let ((content (topic-content topic)))
    (if (and (not (listp content)) (not (null content)))
        (setf *topics* (union *topics* (list content))))))

(defun topic-content (topic)
  (second topic))

(defun normalize (input)
  (dolist (cur *equals*)
    (setf input (substitute-if (second cur) #'(lambda (item) (member item (car cur))) input)))
  input)

(defun use-rules (input rules)
  (some #'(lambda (rule) 
           (let ((result (pat-match (rule-pattern rule) input)))
             (if (not (eq result FAIL))
                 (progn
                   (dolist (topic result)
                     (extend-topics topic))
                   (sublis result (random-elt (rule-responses rule)))))))
        rules))

(defun eliza() 
  (robot *eliza-rules*))

(use-rules '(sorry tom) *eliza-rules*)
(use-rules '(ite) *eliza-rules*)


;(run-tests #'pat-match)
