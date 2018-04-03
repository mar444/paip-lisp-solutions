(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun pat-match (pattern input)
  (cond ((and (null pattern) (null input)) '(nil))
        ((or (null pattern) (null input)) nil)
        (t (let ((current-pattern (first pattern))
                 (current-input (first input))
                 (result (pat-match (rest pattern) (rest input))))
             (cond ((not result) nil)
                   ((variable-p current-pattern)
                    (let ((var (list (cons current-pattern current-input))))
                      (if (null (first result))
                          var
                          (append var result))))
                   ((and (atom current-pattern) (atom current-input) (eql current-pattern current-input))
                    (if (null (first result))
                        '(matched)
                        result))
                   (t f))))))

;; match with ?x
(print (pat-match '(I need f ?X ?Y) '(I need f t f)))

;; match without ?x
(print (pat-match '(I need f) '(I need f)))
(print (pat-match '(I need v f) '(I need v f)))

;; non match
(print (pat-match '(I need f) '(I need f t)))


(print (sublis (pat-match '(I need f ?X ?Y) '(I need f t f))
        '(what would it mean to you if you got a ?X ?Y)))
