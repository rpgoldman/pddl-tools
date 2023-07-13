;;; -------------------------------------------------------------------------
;;; Copyright 2023, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(in-package :hddl-pprinter)

(defvar *hddl-package* :hddl)
(defparameter *hddl-pprint-dispatch* (copy-pprint-dispatch *pddl-pprint-dispatch*))

;;;---------------------------------------------------------------------------
;;; Interface functions
;;;---------------------------------------------------------------------------
(defun hddlify (sym)
  (let ((pddl-io::*pddl-package* hddl-io::*hddl-package*))
    (pddlify sym)))

(defun hddl-symbol (symd)
  (let ((pddl-io::*pddl-package* hddl-io::*hddl-package*))
    (pddl-symbol symd)))


(defun hddlify-tree (tree)
  (let ((pddl-io::*pddl-package* hddl-io::*hddl-package*))
    (pddlify-tree tree)))


(locally (declare #+sbcl (sb-ext:muffle-conditions style-warning))
  (defun pprint-hddl (sexp &optional (stream t) &key (canonical nil))
    "Pretty-print an HDDL plan or domain.
  If the CANONICAL keyword argument is T, then typed lists will be
printed in \"canonical form\", as pairs like:
FOO - TYPE1 BAR - TYPE1 BAZ - TYPE2
instead of \"minimal form\" like
FOO BAR - TYPE1 BAZ - TYPE2."
    (let ((*print-pprint-dispatch* *hddl-pprint-dispatch*)
          (*package* (find-package *hddl-package*))
          (*canonical* canonical)
          *is-problem* *is-domain*)
      (cond ((domain-p sexp) (setf *is-domain* t))
            ((problem-p sexp) (setf *is-problem* t))
            (t (error "Can't determine if argument is problem or domain.")))
      (pprint sexp stream))))

(defun read-hddl-file (dom-prob-file)
  "Takes a domain or problem file and returns its s-expression."
  (let ((*package* (find-package *hddl-package*)))
    (with-open-file (dom-prob dom-prob-file :direction :input)
      (read dom-prob nil nil))))

#|

(defun print-pddl-plan-to-file (sexp filename &optional (if-exists :error))
  "Print a PDDL plan to a file in IPC format that the VAL
plan validator will read."
  (with-open-file (str filename :direction :output :if-exists if-exists)
    (print-pddl-plan sexp str)))

(defun print-pddl-plan (sexp &optional (stream t))
  "Print a PDDL plan in a format that the VAL
validator will read it: one action s-expression per
line."
  (let ((*package* (find-package *pddl-package*))
        (*print-pretty* nil))
    (dolist (step sexp)
      (prin1 step stream)
      (terpri stream))))
|#

(defun read-HDDL-plan-file (filename)
  "Read a HDDL plan from FILENAME and return it
in the form of a list of actions."
  (with-open-file (str filename)
    (read-HDDL-plan str)))

(defun read-HDDL-plan (stream)
  ;; FIXME: rewrite the docstring with return value
  "Read a HDDL plan from STREAM and return it
in the form of a list of actions."
  (let (format root-line actions roots decompositions)
    (let ((*package* (find-package *HDDL-package*)))
      (iter (for line = (read-line stream nil nil))
        (when (null line)
          (error "No plan start string (==>) found."))
        (when (string-equal line "==>")
          (finish))))
    ;; detect the format
    (let ((first-line (read-line stream)))
      (multiple-value-bind (id pos)
          (parse-integer first-line :junk-allowed t)
        (declare (ignore id))
        (setf format
              (cond ((find #\, first-line)
                     :comma-separated)
                    ((eql (aref (string-left-trim (list #\space #\tab) (subseq first-line pos)) 0)
                          #\()
                     :s-expression)
                    (t :default))))
      (setf actions
       (iter (for line initially first-line then (read-line stream))
         (when (string= (subseq line 0 4) "root")
           ;; done reading actions
           (setf root-line line)
           (finish))
         (multiple-value-bind (id pos)
             (parse-integer line :junk-allowed t)
           (collecting
               (cons id
                (ecase format
                  (:comma-separated
                   (let ((components
                           (cl-ppcre:split "," line :start (1+ pos))))
                     (mapcar #'hddl-symbol components)))
                  (:s-expression
                   (let ((*package* *hddl-package*))
                     (read-from-string line t nil :start (1+ pos))))
                  (:default
                   (space-separated-string->hddl-list (subseq line pos))
                   (let ((components
                           (cl-ppcre:split "[ \\t]+" line :start (1+ pos))))
                     (mapcar #'(lambda (x) (intern x *hddl-package*))
                             components))))))))))
    ;; parse the roots
    (when (equalp (subseq (string-left-trim (list #\space #\tab) (subseq root-line 4))
                          0 5)
                  "__top")
      (error "Can't parse HDDL plans with special \"__top\" root task"))
    (setf roots
          (read-integer-list (subseq root-line 4)))

    (setf decompositions
          (iter (for line = (read-line stream nil nil))
            (while (and line
                        (not (string-equal line "<=="))))
            (multiple-value-bind (id pos)
                (parse-integer line :junk-allowed t)
              (multiple-value-bind (task-string method-id subtasks)
                  (partition-method-line (subseq line (1+ pos)))
                (declare (type string task-string method-id subtasks))
                (collecting `(,id
                              ,(space-separated-string->hddl-list task-string)
                              .
                              (,(intern method-id *hddl-package*)
                               ,@(read-integer-list subtasks))))))))
    `(:hddl-plan
      :actions ,actions
      :roots ,roots
      :decompositions ,decompositions)))

(defun read-integer-list (string)
  (iter (with pos = 0)
    (with root)
    (declare (type (or integer null) root pos))
    (multiple-value-setq (root pos)
      (parse-integer string :start pos :junk-allowed t))
    (if root (collecting root)
        (finish))))

(defun space-separated-string->hddl-list (line)
 (let ((components
         (cl-ppcre:split "[ \\t]+" line)))
   (mapcar #'hddl-symbol components)))

;;; helper function for PARSE-HDDL-PLAN -- returns three string values:
;;; the string describing the complex task that's decomposed
;;; the name of the method used to decompose
;;; a list of subtasks
(defun partition-method-line (string)
  (multiple-value-bind (match-start match-end register-starts register-ends)
   (cl-ppcre:scan "^(.*) +-> +(.*)$" string)
    (declare (ignore match-end))
    (unless match-start (error "Unable to parse decomposition description ~a" string))
    (unless (and (= (length register-starts) 2)
                 (= (length register-ends) 2))
      (error "Unable to find blocks in decomposition description ~a" string))
    ;; now divvy up the decomposition side...
    (let ((decomp-component
            (cl-ppcre:split "[ \\t]+"  (subseq string (aref register-starts 1) (aref register-ends 1)))))
      (values (subseq string (aref register-starts 0) (aref register-ends 0))
              (string-left-trim (list #\space #\tab) (first decomp-component))
              (format nil "~{~A~^ ~}" (rest decomp-component))))))

;;;---------------------------------------------------------------------------
;;; Type checkers
;;;---------------------------------------------------------------------------
(defmacro def-type-predicate (name args &body body)
  (let* ((namestring (symbol-name name))
          (suffix-pos (- (length namestring) 2))
          (suffix (subseq namestring suffix-pos))
          (type-name (intern (subseq namestring 0 suffix-pos)
                             (symbol-package name))))
    (assert (equalp "-P" suffix))
    `(progn
      (defun ,name ,args
        ,@body)
      (deftype ,type-name () '(satisfies ,name)))))

(def-type-predicate complex-task-sexp-p (lst)
  (and (eq (first lst) :task)
       (symbolp (second lst))
       (or (= (length lst) 2)           ; no parameters
           (and (eq (third lst) :parameters)
                (typed-list-p (fourth lst))))))

(def-type-predicate method-sexp-p (sexp)
  (and (eq (first sexp) :method)
       (symbolp (second sexp))                 ; method name
       ;; :parameters
       (eq (third sexp) :parameters)
       (typed-list-p (fourth sexp))
       ;; :task
       (eq (fifth sexp) :task)
       (task-sexp-p (sixth sexp))
       ;; optional precondition
       (cond ((eq (seventh sexp) :precondition)
              (and (goal-sexp-p (eighth sexp))
                   (task-network-sexp-p (subseq sexp 8))))
             (t
              ;; task-network is the rest of the sexp
              (task-network-sexp-p (subseq sexp 6))))))

(def-type-predicate task-sexp-p (sexp)
  (and (listp sexp)
       (symbolp (first sexp))
       (every #'symbolp (rest sexp))))

(def-type-predicate task-network-sexp-p (sexp)
  ;; only ordered task networks for now
  (and (member (first sexp) '(:ordered-subtasks :ordered-tasks))
       (every #'task-sexp-p (rest sexp))))

(def-type-predicate goal-sexp-p (sexp)
  ;; not worrying about this for now
  (listp sexp))

;;;---------------------------------------------------------------------------
;;; Pretty-print table entries
;;; The HDDL pretty-print table is simply an extension of the PDDL table.
;;;---------------------------------------------------------------------------

;;; tasks in a domain
(set-pprint-dispatch '(cons (member :task))
                     #'(lambda (str obj)
                         (if (complex-task-def-p obj)
                             (destructuring-bind (keyword task-name
                                                  param-keyword param-defs)
                                 obj
                               (if *canonical*
                                   (let ((sorted-objs
                                           (canonicalize-types param-defs)))
                                     (pprint-logical-block (str obj
                                                                :prefix "("
                                                                :suffix ")")
                                       (format str "~S ~S ~:_~S ~:_" keyword task-name param-keyword)
                                       (pprint-logical-block (str param-defs
                                                                  :prefix "("
                                                                  :suffix ")")
                                         (loop for (sublist . rest) on (min-canonical-sublists sorted-objs)
                                               do
                                                  (pprint-logical-block (str sublist)
                                                    (pprint-indent :block 2 str)
                                                    (let ((entities (reverse (cddr (reverse sublist))))
                                                          (type (first (reverse sublist))))
                                                      (format str "~{~W~^ ~:_~}" entities)
                                                      (format str " - ~W~:_" type)
                                                      (when rest (format str " "))))))))
                                   ;; minimal form, instead of canonical form
                                   (let ((sorted-objs
                                           (minimize-canonical-type-list
                                            (canonicalize-types param-defs))))
                                     (pprint-logical-block (str obj
                                                                :prefix "("
                                                                :suffix ")")
                                       (format str "~S ~S ~:_~s ~:_" keyword task-name param-keyword)
                                       (pprint-logical-block (str param-defs
                                                                  :prefix "(" :suffix ")")
                                         (pprint-indent :block 2 str)
                                         (loop for (sublist . rest) on (min-canonical-sublists sorted-objs)
                                               do (pprint-newline :fill str)
                                               do
                                                  (pprint-logical-block (str sublist)
                                                    (pprint-indent :block 2 str)
                                                    (let ((entities (reverse (cddr (reverse sublist))))
                                                          (type (first (reverse sublist))))
                                                      (format str "~{~W~^ ~:_~}" entities)
                                                      (format str " - ~W~:_" type)
                                                      (when rest (format str " "))))))))))
                             (progn
                               (warn "Ill-formed complex-task-def")
                               (print-object obj str))))
                     0
                     *hddl-pprint-dispatch*)

