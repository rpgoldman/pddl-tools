(in-package :common-lisp-user)

(defun usage (program-name)
  (format t "~a [input-file [output-file]]~%" program-name)
  (format t "Translate input HDDL into output JSON.~%")
  (format t "If no command line arguments are present, reads from stdin and writes to stdout.~%")
  (format t "If one command line argument is present, reads from argument file and writes to the same name, but with \".json\" extension.~%")
  (format t "If two command line arguments are present, reads from first argument file and writes to second argument file.~%"))

(defun main (argv)
  (case (length argv)
    (1 ; just the program name
     (hddl-json:hddl-to-json *standard-input* *standard-output*))
    (2 ; just the input file name
     (let* ((filename (second argv))
            (pathname (parse-namestring filename))
            (outfile (merge-pathnames (make-pathname :type "json")
                                      pathname)))
       (unless (probe-file filename)
         (uiop:die 1 "Unable to open input HDDL file ~a" filename))
       (uiop:with-input-file (in pathname :if-does-not-exist :error)
         (uiop:with-output-file (out outfile :if-exists :supersede)
           (hddl-json:hddl-to-json in out)))))
    (3
     (let ((filename (second argv))
            (outfile (third argv)))
       (unless (probe-file filename)
         (uiop:die 1 "Unable to open input HDDL file ~a" filename))
       (uiop:with-input-file (in filename :if-does-not-exist :error)
         (uiop:with-output-file (out outfile :if-exists :supersede)
           (hddl-json:hddl-to-json in out)))))
    (otherwise
     (usage (first argv))
     (uiop:die 1 "Incorrect invocation: ~a requires 0, 1 or 2 arguments."))))
