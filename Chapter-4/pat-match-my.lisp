(defun variable-p (x)
    (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun pat-match (pattern input)
  (cond ((and (null pattern) (null input)) '(()))
        ((or (null pattern) (null input)) nil)
        (t (let ((current-pattern (first pattern))
              (current-input (first input))
              (result (pat-match (rest pattern) (rest input))))
          (cond ((not result) nil)
                ((variable-p current-pattern)
                 (append (list (list 'variable current-pattern current-input))
                         result))
                ((and (atom current-pattern) (atom current-input) (eql current-pattern current-input))
                 (append '(()) result))
                (t f))))))


(print (pat-match '(I need f ?X) '(I need f t)))
                 
                 