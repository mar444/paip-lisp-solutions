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

(defun non-terminal? (phrase)
    (rewrites phrase))

(defun new-generate (phrase)
  (cond ((listp phrase) (mappend #'new-generate phrase))
        ((non-terminal? phrase) (new-generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(print (new-generate 'sentence))
(print (new-generate 'verb-phrase))