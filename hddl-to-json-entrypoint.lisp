(in-package :common-lisp-user)

(defun main (argv)
  (declare (ignore argv))
  (hddl-json:hddl-to-json *standard-input* *standard-output*))
