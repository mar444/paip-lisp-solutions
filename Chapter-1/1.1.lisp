(defparameter *suffix*
    '(MD Jr.)
    "A list of suffix that can appear at the end of a name.")

(defun last-name (name)
    "Select the last name from a name represented as a list."
    (if (member (first (last name)) *suffix*)
        (last-name (butlast name))
        (first (last name))))