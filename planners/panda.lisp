;;; -------------------------------------------------------------------------
;;; Copyright 2023, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(in-package :sift-pddl-planners)

(defvar *panda-pi-parser* "pandaPIparser")
(defvar *panda-pi-grounder* "pandaPIgrounder")
(defvar *panda-pi-engine* "pandaPIengine")


(defmethod run-planner (domain-file problem-file (planner-key (eql :panda))
                        &key planner-executable output-directory
                          verbose keep-temp
                          (panda-pi-parser *panda-pi-parser*)
                          (panda-pi-grounder *panda-pi-grounder*)
                          (panda-pi-engine *panda-pi-engine*)
                          (timeout-secs (* 10 60)) ; default to 10 minutes
                        &allow-other-keys)
  (declare (ignore planner-executable) (type fixnum timeout-secs))
  (flet ((elapsed-seconds (start end)
           (round (/ (- end start) internal-time-units-per-second))))
    (let* ((output-directory
             (if output-directory
                 (ensure-directories-exist output-directory)
                 (let ((new-pathname (merge-pathnames
                                      (make-pathname :directory `(:relative ,(symbol-name (gensym "panda-workdir"))))
                                      (uiop:temporary-directory))))
                   (ensure-directories-exist new-pathname))))
           (*default-pathname-defaults* output-directory)
           (remaining-time timeout-secs))
      (let ((parsed-file (namestring (merge-pathnames "parsed.htn")))
            (grounded-file (namestring (merge-pathnames "grounded.panda")))
            (raw-plan (namestring (merge-pathnames "plan.panda")))
            (hddl-plan (namestring (merge-pathnames "plan.hddl")))
            (start-time (get-internal-run-time)))

        (multiple-value-bind (output error-output success)
            (uiop:run-program (list "timeout"
                                    ;; "--signal=KILL"
                                    (format nil "~ds" remaining-time)
                                    panda-pi-parser
                                    (namestring domain-file)
                                    (namestring problem-file)
                                    parsed-file)
                              :ignore-error-status t
                              :error-output :output
                              :output :string)
          (declare (ignore error-output))
          (cond ((= success 124)
                 (error "Timed out while parsing the domain and/or problem"))
                ((zerop success))
                (t
                 (error "Failed to parse the domain and/or problem with exit code ~d.~%Output:~%~a" success output))))
        (decf remaining-time (elapsed-seconds start-time (get-internal-run-time)))
        (when (< remaining-time 0) (error "Timeout during parsing."))
        (when verbose (format t "~&Done parsing HDDL inputs.~%"))
        (setf start-time (get-internal-run-time))
        (multiple-value-bind (output error-output success)
            (uiop:run-program (list "timeout"
                                    ;; "--signal=KILL"
                                    (format nil "~ds" remaining-time)
                                    panda-pi-grounder
                                    parsed-file
                                    grounded-file)
                              :ignore-error-status t
                              :error-output :output
                              :output :string)
          (declare (ignore error-output))
          (decf remaining-time (elapsed-seconds start-time (get-internal-run-time)))
          (cond ((= success 124)
                 (error "Timed out while grounding the domain and problem"))
                ((zerop success))
                (t
                 (error "Failed to ground the domain and problem with exit code ~d.~%Output:~%~a" success output)))
          (when (search "unreachable" output)
            (error "Panda grounder proved the problem unsolveable.~%Output:~%~a"  output)))
        (when (< remaining-time 0) (error "Timeout during grounding."))
        (when verbose (format t "~&Done grounding Panda problem.~%"))

        (setf start-time (get-internal-run-time))
        (multiple-value-bind (output error-output success)
            (uiop:run-program (list panda-pi-engine
                                    (format nil "--timelimit=~d" remaining-time)
                                    grounded-file)
                              :ignore-error-status t
                              :error-output :output
                              :if-output-exists :supersede
                              :output raw-plan)
          (declare (ignore output error-output))
          (decf remaining-time (elapsed-seconds start-time (get-internal-run-time)))
          (when (< remaining-time 0) (error "Timeout during planning."))
          (cond ((or (= success 124) (= success 137))
                 (error "Timed out while finding a solution for the problem (pandaPIengine)"))
                ((zerop success))
                (t
                 (error "Failed to solve the problem with exit code ~d.~%Output:~%~a" success (alexandria:read-file-into-string raw-plan)))))
        (when verbose (format t "~&Done solving Panda problem.~%"))
        (multiple-value-bind (output error-output success)
            (uiop:run-program (list panda-pi-parser "-c" raw-plan hddl-plan)
                              :ignore-error-status t
                              :error-output :output
                              :output :string)
          (declare (ignore error-output))
          (unless (zerop success)
            (error "Failed to translate the plan into HDDL with exit code ~d.~%Output:~%~a" success output)))
        (when verbose (format t "~&Done translating Panda plan to HDDL.~%"))
        (values (uiop:with-input-file (str hddl-plan)
                  (hddl-io:read-hddl-plan str))
                (when keep-temp output-directory))))))
