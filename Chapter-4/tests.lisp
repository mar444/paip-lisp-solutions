(defun run-school-tests (gps *ops*)
    ;; 4.4 TEST CASES

    ;; SOLVED 
    (print (gps '(SON-AT-HOME CAR-NEEDS-BATTERY HAVE-MONEY HAVE-PHONE-BOOK) '(SON-AT-SCHOOL) *ops*))

    ;; NIL
    (print (gps '(SON-AT-HOME CAR-NEEDS-BATTERY HAVE-MONEY) '(SON-AT-SCHOOL) *ops*))

    ;; SOLVED
    (print (gps '(SON-AT-HOME CAR-WORKS) '(SON-AT-SCHOOL) *ops*))
    
    
    ;; 4.7 TEST CASES 
    
    ;; SOLVED
    (print (GPS '(SON-AT-HOME HAVE-MONEY CAR-WORKS) '(HAVE-MONEY SON-AT-SCHOOL) *ops*))

    ;; NIL
    (print (GPS '(SON-AT-HOME CAR-NEEDS-BATTERY HAVE-MONEY HAVE-PHONE-BOOK) '(SON-AT-SCHOOL HAVE-MONEY) *ops*))


    ;; 4.9 TEST CASES


    (push (make-op :action 'ask-phone-number
                   :preconds '(in-communication-with-shop)
                   :add-list '(know-phone-number))
          *school-ops*)

    ;; NIL
    (print (GPS '(SON-AT-HOME CAR-NEEDS-BATTERY HAVE-MONEY) '(SON-AT-SCHOOL) *ops*)))


(defun run-blocks-tests (gps *ops*)
    (print 
      (GPS '((a on table) (b on table) (space on a) (space on b) (space on table))
         '((a on b) (b on table)) *ops*))
    
    (print 
      (GPS '((a on b) (b on table) (space on a) (space on table))
         '((b on a)) *ops*))
    
    (print
      (GPS '((a on b) (b on c) (c on table) (space on a) (space on table))
         '((b on a) (c on b)) *ops*))
    
    (print
      (GPS '((c on a) (a on table) (b on table) (space on c) (space on b) (space on table))
         '((b on c) (a on b)) *ops*)))
