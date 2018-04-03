(defun run-tests (pat-match)

  ;; match with ?x
  (print (pat-match '(I need f ?X ?Y) '(I need f t f)))
  (print (pat-match '(I need f ?X ?X) '(I need f t t)))
  (print (pat-match '(?X is ?X) '((2 + 2) is (2 + 2))))
  
  ;; match without ?x
  (print (pat-match '(I need f) '(I need f)))
  (print (pat-match '(I need v f) '(I need v f)))
  
  ;; non match
  (print (pat-match '(I need f) '(I need f t)))
  (print (pat-match '(I need f ?X ?X) '(I need f t f)))
  
  
  (print (sublis (pat-match '(I need f ?X ?Y) '(I need f t f))
                 '(what would it mean to you if you got a ?X ?Y))))