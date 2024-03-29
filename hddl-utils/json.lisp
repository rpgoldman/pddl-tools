;;;---------------------------------------------------------------------------
;;; Copyright (c) 2023 Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; and Robert P. Goldman
;;; All rights reserved.
;;;
;;;---------------------------------------------------------------------------
;;; File Description:  Code to serialize HDDL domain and problem files as
;;; JSON using the schemas attached to this library.
;;;
;;;
;;;---------------------------------------------------------------------------


(defpackage hddl-json
  (:use common-lisp hddl-utils cl-json iterate)
  (:nicknames #:hddl-to-json)
  (:import-from #:hddl
                #:forall #:exists #:imply)
  (:import-from #:pddl-utils #:flatten-conjunction)
  (:export #:hddl-to-json
           #:json-dump-domain
           #:json-dump-problem))

(in-package :hddl-json)

(defvar *hddl-encoding*
  nil
  "Should be set to true when encoding HDDL as JSON so that symbols
  are serialized correctly.")

(defparameter +domain-schema+ "https://www.sift.net/hddl/draft/2023-07-31/domain")
(defparameter +problem-schema+ "https://www.sift.net/hddl/draft/2023-07-31/problem")

(defun hddl-to-json (&optional (input-stream *standard-input*)
                       (output-stream *standard-output*))
  (let ((hddl-entity (hddl-io:read-hddl-stream input-stream)))
    (etypecase hddl-entity
      (hddl:domain (json-dump-domain hddl-entity output-stream))
      (hddl:problem (json-dump-problem hddl-entity output-stream)))))

;;; When encoding a symbol, use the normal camel case output for
;;; keys in dictionaries, but simply downcase symbols for values
;;; in dictionaries -- which are the actual expressions in the source
;;; HDDL.
(defmethod json:encode-json :around ((s symbol) &optional (stream *json-output*))
  (if *hddl-encoding*
      (if (keywordp s)
          (call-next-method)
          (let ((s (string-downcase (symbol-name s))))
            (json::write-json-string s stream)))
      (call-next-method)))

(defun jsonify-sym (sym)
  "Alternative way of translating CL symbols to JSON strings.
We keep the old method for keywords, which are used as property names,
and should be CamelCase.  But for symbols used in definitions (e.g., task
or predicate names), symbols should just be turned into down-cased strings,
and notably hyphens should not be replaced."
  (if (keywordp sym)
      (lisp-to-camel-case sym)
      (string-downcase sym)))

(defun json-dump-domain (domain &optional (stream *json-output*))
  "Print a JSON representation of DOMAIN to STREAM."
  (let ((*json-output* stream)
        (*hddl-encoding* t))
    (with-object ()
      (encode-object-member '#:|$schema| +domain-schema+)
      (encode-object-member 'name (string-downcase (symbol-name (domain-name domain))))
      (as-object-member (:types)
        (with-array ()
          (let ((alist
                  (hddl-utils:typelist-to-alist (domain-types domain))))
            (iter (for (type . supertype) in alist)
              (as-array-member ()
                (with-object ()
                  (encode-object-member :type type)
                  (encode-object-member :supertype supertype)))))))
      (as-object-member (:requirements)
        (let ((*lisp-identifier-name-to-json* #'(lambda (i) (string-downcase i))))
          (encode-json (domain-requirements domain))))
      (as-object-member (:constants)
        (with-array ()
          (let ((alist
                  (hddl-utils:typelist-to-alist (domain-constants domain))))
            (iter (for (constant . type) in alist)
              (as-array-member ()
                (with-object ()
                  (encode-object-member :name constant)
                  (encode-object-member :type type)))))))
      (as-object-member (:predicates)
        (with-array ()
          (iter (for pred-def in (domain-predicates domain))
            (as name = (predicate-name pred-def))
            (as params = (predicate-parameters pred-def))
            (as-array-member ()
              (with-object ()
                (encode-object-member :name name)
                (as-object-member (:parameters)
                  (let ((alist (typelist-to-alist params)))
                    (with-array ()
                      (iter (for (param . type) in alist)
                        (as-array-member ()
                          (with-object ()
                            (encode-object-member :name param)
                            (encode-object-member :type type))))))))))))
      (as-object-member (:tasks)
        (with-array ()
          (iter (for pred-def in (domain-tasks domain))
            (as name = (task-name pred-def))
            (as params = (task-parameters pred-def))
            (as-array-member ()
              (with-object ()
                (encode-object-member :name name)
                (as-object-member (:parameters)
                  (let ((alist (typelist-to-alist params)))
                    (with-array ()
                      (iter (for (param . type) in alist)
                        (as-array-member ()
                          (with-object ()
                            (encode-object-member :name param)
                            (encode-object-member :type type))))))))))))
      (as-object-member (:actions)
        (with-array ()
          (dolist (x (domain-actions domain))
            (as-array-member ()
              (json-dump-action x stream)))))
      (as-object-member (:methods)
        (with-array ()
          (dolist (x (domain-methods domain))
            (as-array-member ()
              (json-dump-method x stream)))))
      (finish-output stream)
      (values))))

(defun json-dump-problem (problem &optional (stream *json-output*))
  "Print a JSON representation of PROBLEM to STREAM."
  (let ((*json-output* stream)
        (*hddl-encoding* t))
    (with-object ()
      (encode-object-member '#:|$schema| +problem-schema+)
      (encode-object-member 'name (string-downcase (symbol-name (problem-name problem))))
      (encode-object-member 'domain (string-downcase (symbol-name (problem-domain problem))))
      (when (problem-requirements problem)
        (as-object-member (:requirements)
          (let ((*lisp-identifier-name-to-json* #'(lambda (i) (string-downcase i))))
            (encode-json (problem-requirements problem)))))
      (as-object-member (:objects)
        (with-array ()
          (let ((alist
                  (hddl-utils:typelist-to-alist
                   (pddl-utils:canonicalize-types
                    (problem-objects problem)))))
            (iter (for (constant . type) in alist)
              (as-array-member ()
                (with-object ()
                  (encode-object-member :name constant)
                  (encode-object-member :type type)))))))
      (as-object-member (:init)
        (with-array ()
          (iter (for fact in (problem-state problem))
            (as-array-member ()
              (json-dump-atom fact stream)))))
      (when (problem-goal problem)
       (as-object-member (:goal)
         (json-dump-goal (problem-goal problem) stream)))
      (as-object-member (:htn)
        (json-dump-htn (problem-htn problem) stream))
      (values))))

(defun json-dump-action (action &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
    (with-object ()
      (encode-object-member :name (action-name action))
      (as-object-member (:parameters)
        (json-dump-typelist (action-params action) stream))
      (when (action-precondition action)
       (as-object-member (:precondition)
         (json-dump-goal (action-precondition action) stream)))
      (unless (null (action-effect action))
       (as-object-member (:effect)
         (json-dump-effect (action-effect action) stream))))))

(defun json-dump-method (method &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
    (with-object ()
      (encode-object-member :name (method-name method))
      (as-object-member (:parameters)
        (json-dump-typelist (method-parameters method) stream))
      (as-object-member (:task)
        (json-dump-task (method-task method) stream))
      (when (method-precondition method)
       (as-object-member (:precondition)
         (json-dump-goal (method-precondition method) stream)))
      (as-object-member (:task-network)
        (if (ordered-method-p method)
            (json-dump-ordered-subtasks (method-subtasks method) stream)
            (error "JSON serialization does not handle partially-ordered HTNs."))))))

(defun json-dump-goal (goal &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
    (if (null goal)
        (json:encode-json goal stream)
        (case (first goal)
          ((or and)
           ;; n-ary operators
           (json-dump-nary goal stream))
          (not
           (json-dump-negation goal stream))
          (imply
           (json-dump-binary goal stream))
          ((forall exists)
           (json-dump-quantified goal stream))
          (otherwise
           (json-dump-atom goal stream)
           )))))

(defun json-dump-atom (atom &optional (stream *json-output*))
  "Dump ATOM as JSON object, treating it as a PDDL or HDDL atomic formula,
with \"predicate\" and \"args\" (array) components."
  (let ((*json-output* stream)
        (*hddl-encoding* t))
    (with-object (stream)
      (encode-object-member :predicate (first atom) stream)
      (as-object-member (:args stream)
        (with-array (stream)
          (dolist (x (rest atom))
            (encode-array-member x stream)))))))

(defun json-dump-task (task &optional (stream *json-output*))
  "Dump TASK as JSON object, treating it as a PDDL or HDDL atomic task,
with \"taskName\" and \"args\" (array) components."
  (let ((*json-output* stream)
        (*hddl-encoding* t))
    (with-object (stream)
      (encode-object-member :task-name (first task) stream)
      (as-object-member (:args)
        (with-array ()
          (dolist (x (rest task))
            (encode-array-member x stream)))))))


(defun json-dump-negation (goal &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
   (with-object ()
     (encode-object-member :op 'not)
     (as-object-member (:operand)
       (json-dump-atom (second goal))))))

(defun json-dump-nary (goal &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
   (with-object ()
     (encode-object-member :op (first goal))
     (as-object-member (:operands)
       (with-array ()
         (dolist (x (rest goal))
           (as-array-member ()
             (json-dump-goal x))))))))

(defun json-dump-typelist (typelist &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
    (let ((alist (typelist-to-alist (hddl-utils:canonicalize-types typelist))))
     (with-array ()
       (iter (for (name . type) in alist)
         (as-array-member ()
           (with-object ()
             (encode-object-member :name name)
             (encode-object-member :type type))))))))


(defun json-dump-binary (goal &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
   (with-object ()
     (encode-object-member :op (first goal))
     (as-object-member (:operand1)
       (json-dump-goal (second goal)))
     (as-object-member (:operand2)
       (json-dump-goal (third goal))))))

(defun json-dump-quantified (goal &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
   (with-object ()
     (encode-object-member :op (first goal))
     (as-object-member (:bound-vars)
       (json-dump-typelist (second goal) stream))
     (as-object-member (:operand)
       (json-dump-goal (third goal))))))

(defun json-dump-effect (effect &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
    (unless (null effect)
      (case (first effect)
        (and
         ;; n-ary operators
         (json-dump-conj-effect effect stream))
        (not
         (json-dump-negated-effect effect stream))
        (when
            (json-dump-cond-effect effect stream))
        ((forall exists)
         (json-dump-quantified-effect effect stream))
        (otherwise
         (json-dump-atom effect stream))))))

(defun json-dump-conj-effect (effect &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
   (with-object ()
     (encode-object-member :op 'and)
     (as-object-member (:effects)
       (with-array ()
         (dolist (x (rest effect))
           (as-array-member ()
             (json-dump-effect x stream))))))))


(defun json-dump-negated-effect (goal &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
   (with-object ()
     (encode-object-member :op 'not)
     (as-object-member (:effect)
       (json-dump-effect (second goal) stream)))))

(defun json-dump-cond-effect (goal &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
   (with-object ()
     (encode-object-member :op :when)
     (as-object-member (:condition)
       (json-dump-goal (second goal)))
     (as-object-member (:effect)
       (json-dump-effect (third goal))))))

(defun json-dump-quantified-effect (effect &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
   (with-object ()
     (encode-object-member :op (first effect))
     (as-object-member (:bound-vars)
       (json-dump-typelist (second effect) stream))
     (as-object-member (:effect)
       (json-dump-effect (third effect))))))

(defun json-dump-ordered-subtasks (subtask-conj &optional (stream *json-output*) (as-object t))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
    (flet ((dump-subtasks ()
             (as-object-member (:ordered-subtasks)
               (with-array ()
                 (let ((flattened (flatten-conjunction subtask-conj nil)))
                   (when (eq (first flattened) 'and)
                     (setf flattened (rest flattened)))
                   (dolist (x flattened)
                     (as-array-member ()
                                      (json-dump-task x stream))))))))
      (if as-object
          (with-object ()
            (dump-subtasks))
          (dump-subtasks)))))

(defun json-dump-htn (htn &optional (stream *json-output*))
  (let ((*json-output* stream)
        (*hddl-encoding* t))
    (assert (typep htn 'p-htn))
    (with-object ()
      (as-object-member (:parameters)
        (json-dump-typelist (hddl-utils:canonicalize-types (p-htn-parameters htn)) stream))
      (if (hddl-utils::ordered-task-net-p htn)
          (json-dump-ordered-subtasks (p-htn-ordered-subtasks htn) stream nil)
          (error "Translating partially-ordered problem-HTN's not yet implemented")))))
