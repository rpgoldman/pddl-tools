;;; -------------------------------------------------------------------------
;;; Copyright 2008-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(in-package :sift-pddl-planners)

(defvar *planning-outdir*
  #+allegro (sys:temporary-directory)
  #-allegro
  (uiop:temporary-directory))

(defgeneric run-planner (domain-file problem-file planner-key
                         &key planner-executable output-directory
                           verbose
                         &allow-other-keys)
  (:documentation "Invoke a PDDL planner program, designated by PLANNER-KEY, on
  DOMAIN-FILE and PROBLEM-FILE.  The planner library will know how to find the
  executable, but this can be overridden using the PLANNER-EXECUTABLE keyword
  argument.  The function will return a plan as an s-expression in the PDDL
  library, but there may additionally be output files, which will be written
  into OUTPUT-DIRECTORY.  If VERBOSE is non-NIL, diagnostic information will be
  printed.")
  (:method :around (domain-file problem-file planner-key
                    &rest args
                    &key
                      (output-directory *planning-outdir*)
                    &allow-other-keys)
    "Around method that checks for error conditions and supplies default OUTPUT-DIRECTORY."
    (let ((domain-file (namestring (merge-pathnames domain-file
                                                    *default-pathname-defaults*)))
          (problem-file (namestring (merge-pathnames problem-file
                                                     *default-pathname-defaults*)))
          (new-args (copy-list args)))
      (remf new-args :output-directory)

      (unless (probe-file domain-file)
        (error 'file-error :pathname domain-file))

      (unless (probe-file problem-file)
        (error 'file-error :pathname problem-file))

      (unless (probe-file output-directory)
        (error "No such directory: ~a" output-directory))

      (apply #'call-next-method
             domain-file
             problem-file
             planner-key
             :output-directory output-directory
             new-args))))


(defgeneric validate (validator-key domain-file &optional problem-file plan-file
                        &key
                          verbose)
  (:documentation "Check to see if the DOMAIN-FILE \(and optionally PROBLEM-FILE
and PLAN-FILE\) are valid PDDL.  Return NIL if the check fails, else T.
The method for checking is controlled by VALIDATOR-KEY."))

(defgeneric read-plan-file (filename planner-key)
  (:documentation "Takes a file produced by the planner corresponding to PLANNER-KEY and returns its s-expression
\(made up of symbols in the PDDL package\) as a list."))

(defmethod read-plan-file (path-filename (planner-key (eql ':plain)))
  "Takes a file where each line contains an action head and returs its s-expression
   as a list."
  (let ((*package* (find-package :pddl)))
    (with-open-file
     (str path-filename :direction :input)
     (loop for x = (read str nil)
           while x
           collect x))))

(defgeneric string-to-action (package-name string planner-name)
  (:documentation
  "Take STRING, and turn it into an s-expression in PACKAGE-NAME representing
a step in a PDDL plan.  The PLANNER-NAME argument may be used to figure
out how properly to interpret the string.
  If the STRING represents meta-information, this function may return NIL
instead of returning a step to be added to the plan."))



