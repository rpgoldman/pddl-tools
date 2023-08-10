;;; -------------------------------------------------------------------------
;;; Copyright 2023, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(in-package :sift-pddl-planners)


(defmethod run-planner (domain-file problem-file (planner-key (eql :panda))
                        &key planner-executable output-directory
                          verbose keep-temp
                          (panda-pi-parser "pandaPIparser")
                          (panda-pi-grounder "pandaPIgrounder")
                          (panda-pi-engine "pandaPIengine")
                        &allow-other-keys)
  (declare (ignore planner-executable))
  (let* ((output-directory
           (if output-directory
               (ensure-directories-exist output-directory)
               (let ((new-pathname (merge-pathnames
                                    (make-pathname `(:relative ,(symbol-name (gensym "panda-workdir"))))
                                    (uiop:temporary-directory))))
                 (ensure-directories-exist new-pathname))))
         (*default-pathname-defaults* output-directory))
    (let ((parsed-file (namestring (merge-pathnames "parsed.htn")))
          (grounded-file (namestring (merge-pathnames "grounded.panda")))
          (raw-plan (namestring (merge-pathnames "plan.panda")))
          (hddl-plan (namestring (merge-pathnames "plan.hddl"))))

      (multiple-value-bind (output error-output success)
          (uiop:run-program (list panda-pi-parser
                                  (namestring domain-file)
                                  (namestring problem-file)
                                  parsed-file)
                            :ignore-error-status t
                            :error-output :output
                            :output :string)
        (declare (ignore error-output))
        (unless (zerop success)
          (error "Failed to parse the domain and/or problem with exit code ~d.~%Output:~%~a" success output)))
      (when verbose (format t "~&Done parsing HDDL inputs.~%"))
      (multiple-value-bind (output error-output success)
          (uiop:run-program (list panda-pi-grounder
                                  parsed-file
                                  grounded-file)
                            :ignore-error-status t
                            :error-output :output
                            :output :string)
        (declare (ignore error-output))
        (unless (zerop success)
          (error "Failed to ground the problem with exit code ~d.~%Output:~%~a" success output))
        (when (search "unreachable" output)
                    (error "Panda grounder proved the problem unsolveable.~%Output:~%~a"  output)))
      (when verbose (format t "~&Done grounding Panda problem.~%"))
      (multiple-value-bind (output error-output success)
          (uiop:run-program (list panda-pi-engine grounded-file)
                            :ignore-error-status t
                            :error-output :string
                            :output raw-plan)
        (declare (ignore output))
        (unless (zerop success)
          (error "Failed to solve the problem with exit code ~d.~%Output:~%~a" success error-output)))
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
              (when keep-temp output-directory)))))
