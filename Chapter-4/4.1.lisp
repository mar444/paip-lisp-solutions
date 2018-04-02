(load "auxfns.lisp")

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (apply #'format *debug-io* (concatenate 'string "~%" format-string) args)))

(debug :gps)
(dbg :gps "~a" 2)