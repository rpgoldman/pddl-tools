;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------
;;;
;;; File Description:
;;;
;;;    File that contains the WITH-PROBLEM and AUGMENTING-PROBLEM macros and
;;; their supporting code and macros...
;;;
;;;---------------------------------------------------------------------------

(in-package :sift-pddl-utils)

(defvar *preds*)
(defvar *constants*)
(defvar *facts*)

(defmacro with-problem ((name &key domain
                                goal
                                requirements
                                ) &body body)
  "Define a new problem with NAME, DOMAIN, GOAL and REQUIREMENTS
as given in the keyword arguments, and with additions from BODY.
Return the PDDL s-expression that results."
  `(let (*preds* *constants* *facts*)
     ,@body
     (let ((new-problem (make-problem ',name
                                      :domain ',domain
                                      :objects *constants*
                                      :init *facts*
                                      :goal ',goal
                                      ,@(when requirements
                                          `((:requirements ',requirements))))))

       new-problem)))

(defmacro augmenting-problem ((problem &key domain goal requirements) &body body)
  "Create and return PDDL for a new problem defined by taking PROBLEM \(possibly
with modified DOMAIN, GOAL and/or REQUIREMENTS\), and then augmenting it by
BODY."
  (let ((probvar (gensym)))
    `(let ((,probvar (copy-tree ,problem)))
       ,@(when domain
               `((setf (problem-domain ,probvar) ',domain)))
       ,@(when goal
               `((setf (problem-goal ,probvar) ',goal)))
       ,@(when requirements
               `((setf (problem-requirements ,probvar) ',requirements)))
       (let (*facts* *constants*)
         ,@body
         (setf (problem-state ,probvar)
               (append (problem-state ,probvar)
                       *facts*)
               (problem-objects ,probvar)
               (append (problem-objects ,probvar)
                       *constants*)))
       ,probvar)))

(defmacro definstance ((name &optional (type 'pddl:object)) &body facts)
  "This macro is for use within WITH-PROBLEM or AUGMENTING-PROBLEM.
Define a new instance \(constant\) with NAME of TYPE, and predicate
FACTS."
  ;; FIXME: should modify this by reformulating it using MAKE-INST
  (when (stringp (first facts))
    ;; documentation string... discard for now.
    (pop facts))
  `(progn
     (setf *constants* (append *constants* '(,name - ,type)))
     (setf *facts* (append *facts* ',facts))
     ',name))

(defun make-inst (name type &optional facts)
  "Functional form for DEFINSTANCE.  For use as a building block
for code to run in the body of WITH-PROBLEM or AUGMENTING-PROBLEM."
  (setf *constants* (append *constants* `(,name - ,type)))
  (setf *facts* (append *facts* facts))
  name)

(defun add-fact (fact)
  "For use within WITH-PROBLEM or AUGMENTING-PROBLEM.  Programmer
interface to add FACT to an accumulator.  For most uses, the FACTS
macro will be more convenient."
  (setf *facts* (append *facts* (list fact))))

(defmacro facts (&rest facts)
  "Macro for use within WITH-PROBLEM or AUGMENTING-PROBLEM.  Add FACTS
to the initial state of the current problem."
 `(progn
    ,@(iter (for fact in facts)
            (collecting `(add-fact ',fact)))))
