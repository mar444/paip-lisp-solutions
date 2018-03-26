(load "auxfns.lisp")
(load "simple.lisp")


;; previous version
; (defun generate (phrase)
;   "Generate a random sentence or phrase"
;   (cond ((listp phrase)
;          (mappend #'generate phrase))
;         ((rewrites phrase)
;          (generate (random-elt (rewrites phrase))))
;         (t (list phrase))))

(defun new-generate (phrase)
  (cond ((listp phrase) (mappend #'new-generate phrase))
        (t (let ((result (rewrites phrase)))
             (if result
                 (new-generate (random-elt result))
                 (list phrase))))))

(print (new-generate 'sentence))
(print (new-generate 'verb-phrase))