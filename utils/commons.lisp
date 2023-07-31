;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(in-package :pddl-utils)

(deftype domain ()
  `(and list (satisfies domain-sexp-p)))

(deftype problem ()
  `(and list (satisfies problem-sexp-p)))

(deftype action ()
  `(and list (satisfies action-sexp-p)))


(defun domain-p (x)
  (typep x 'domain))

(defun action-p (x)
  "Simple test to see if an s-expression is a PDDL action."
  (typep x 'action))


(defun problem-p (x)
  (typep x 'problem))

(defun domain-sexp-p (domain-expr)
  (and (eq (first domain-expr) (pddl-symbol 'pddl:define))
       (eq (first (second domain-expr)) (pddl-symbol 'pddl:domain))))

(defun problem-sexp-p (problem-expr)
  (and (eq (first problem-expr) (pddl-symbol 'pddl:define))
       (eq (first (second problem-expr)) (pddl-symbol 'pddl:problem))))

(defun action-sexp-p (problem-expr)
  (eq (first problem-expr) :action))

(defun durative-action-sexp-p (problem-expr)
  (eq (first problem-expr) :durative-action))

;; Primitives:
(defun problem-element (problem pddl-keyword)
  (assert (problem-p problem))
  (let ((head (find-if #'(lambda(e)
                           (and (listp e) (eq (car e) pddl-keyword)))
                       problem)))
    (rest head)))

;; Getter/setter functions for PDDL expressions:
(defun domain-element (domain-expr pddl-keyword)
  (assert (domain-p domain-expr))
  (let ((head (find-if #'(lambda(e)
                           (and (listp e) (eq (car e) pddl-keyword)))
                       domain-expr)))
    (rest head)))

(defun has-element-p (domain-expr pddl-keyword)
  (assert (domain-p domain-expr))
  (find-if #'(lambda(e)
               (and (listp e) (eq (car e) pddl-keyword)))
           domain-expr))

;; makers
(defun make-domain (name &key (requirements '(:adl)) constants predicates actions
                    types)
  (let ((constants (pddlify-tree constants))
        (predicates (pddlify-tree predicates))
        (actions (pddlify-tree actions))
        (types (pddlify-tree types))
        (functions (when (member :action-costs requirements)
                     (pddlify-tree '((total-cost) - number)))))
    (if (member :durative-actions requirements)
        (assert (every #'durative-action-sexp-p actions))
        (assert (every #'action-sexp-p actions)))
    `(,(pddl-symbol 'pddl:define) (,(pddl-symbol 'pddl:domain) ,(pddlify name))
         (:requirements ,@requirements)
       (:types ,@types)
       (:constants ,@constants)
       (:predicates ,@predicates)
       ,@(when functions `((:functions ,@functions)))
       ,@actions)))

(defun canonicalize-domain (old-domain)
  (let ((requirements
          (progn
            (unless (has-element-p old-domain :requirements)
              (error "No requirements in domain.  Don't know how to handle it."))
            (domain-element old-domain :requirements)))
        (types
          (when (has-element-p old-domain :types)
            (domain-types old-domain)))
        (predicates
          (when (has-element-p old-domain :predicates)
            (domain-predicates old-domain)))
        (constants
          (when (has-element-p old-domain :constants)
            (domain-constants old-domain)))
        (actions (domain-actions old-domain)))
    (make-domain (domain-name old-domain)
             :requirements requirements
             :types types
             :constants constants
             :predicates predicates
             :actions actions)))

(defun make-problem (name &key requirements domain objects init goal
                     (complete-p t))
  "Make a new PDDL problem s-expression initialized as per the keyword
arguments.  Unless COMPLETE-P is NIL, will check for mandatory components."
  (when complete-p
    (unless domain (error "DOMAIN argument is mandatory."))
    (unless goal (error "GOAL argument is mandatory.")))
  (flet ((negated (fact) (eq (first fact) 'not)))
    (let ((domain (pddlify domain))
          (objects (pddlify-tree objects))
          (init (pddlify-tree init))
          (goal (pddlify-tree goal)))
      (when (some #'negated init)
        (cerror "Remove negated initial facts."
                "Negated facts in :init are unnecessary and may break some planners.")
        (setf init (remove-if #'negated init)))
      (unless (= (length init) (length (remove-duplicates init :test 'equal)))
        (cerror "Remove duplicates"
                "Some duplicated facts in init.  This is known to break some planners.")
        (setf init (remove-duplicates init :test 'equal)))
      `(,(pddl-symbol 'pddl:define) (,(pddl-symbol 'pddl:problem) ,(pddlify name))
           (:domain ,domain)
         ,@(when requirements
                `((:requirements ,@requirements)))
         (:objects ,@objects)
         (:init ,@init)
         (:goal ,goal)))))

(defmethod copy-domain ((domain list))
  (copy-tree domain))

(defmethod copy-problem ((problem list))
  (copy-tree problem))


;;; misc utility
(defun pddlify (sym &optional (package *pddl-package*))
  (cond ((eq (symbol-package sym) (find-package package))
         sym)
        ((eq (symbol-package sym) (find-package :keyword))
         sym)
        (t
         (pddl-symbol sym))))

(defun pddlify-tree (tree &optional (package *pddl-package*))
  (let ((*pddl-package* package))
   (cond ((null tree) nil)
         ((symbolp tree)
          (pddlify tree))
         ((consp tree)
          (cons (pddlify-tree (car tree)) (pddlify-tree (cdr tree))))
         (t tree))))


(defun problem-domain (problem)
  (let ((element
         (problem-element problem :domain)))
    (assert (= (length element) 1))
    (first element)))

(defsetf problem-domain (problem) (domain)
  `(progn
     (assert (problem-p ,problem))
     (let ((head (find-if #'(lambda(e)
                              (and (listp e) (eq (car e) :domain)))
                          ,problem)))
       (assert (= (length head) 2))
       (setf (second head) (pddlify-tree ,domain)))))

(defun problem-objects (problem)
  (problem-element problem :objects))

(defsetf problem-objects (problem) (objectslist)
  `(progn
     (assert (problem-p ,problem))
     (setf
      (cdr (find :objects (cddr ,problem) :key 'first))
      (pddlify-tree ,objectslist))))

(defun problem-requirements (problem)
  (problem-element problem :requirements))

(defsetf problem-requirements (problem) (requirementslist)
  `(progn
     (assert (problem-p ,problem))
     (setf
      (cdr (find :requirements (cddr ,problem) :key 'first))
      (pddlify-tree ,requirementslist))))


(defun problem-state (problem)
  (problem-element problem :init))

(defsetf problem-state (problem) (statelist)
  `(progn
     (assert (problem-p ,problem))
     (setf
      (cdr (find :init (cddr ,problem) :key 'first))
      (pddlify-tree ,statelist))))

(defun problem-goal (prob)
  (assert (problem-p prob))
  (second
   (find :goal (cddr prob) :key 'first)))

(defmethod domain-name ((domain list))
  (assert (domain-p domain))
  (second (second domain)))

(defmethod (setf domain-name) ((val symbol) (domain list))
  (assert (domain-p domain))
  (setf (cadadr domain) (pddlify-tree val)))

(defmethod problem-name ((problem list))
  (assert (problem-p problem))
  (second (second problem)))

(defmethod (setf problem-name) ((val symbol) (problem list))
  (assert (problem-p problem))
  (setf (cadadr problem) (pddlify-tree val)))

(defsetf problem-goal (prob) (goal)
  `(progn
     (assert (problem-p ,prob))
     (alexandria:if-let ((cell (find :goal (cddr ,prob) :key 'first)))
       ;; if there's already a goal, replace it
       (setf (cdr cell)
             (pddlify-tree
              (list ,goal)))
       ;; otherwise add a new goal
       (nconc ,prob (list (list :goal ,goal))))
     ,goal))

(defun domain-predicates (domain)
  (assert (domain-p domain))
  (let ((head (find-if #'(lambda (e)
                           (and (listp e) (eq (car e) ':predicates)))
                       domain)))
    (rest head)))

(defsetf domain-predicates (domain) (new-pred-list)
  `(progn
     (assert (domain-p ,domain))
     (setf
      (cdr (find :predicates (cddr ,domain) :key 'first))
      (pddlify-tree
       (remove-duplicates
        (append (domain-predicates ,domain) ,new-pred-list))))))

(defun domain-constants (domain)
  (assert (domain-p domain))
  (domain-element domain :constants))

;;; FIXME: this assumes that the domain is typed.  Should handle untyped
;;; domains, as well.
(defun (setf domain-constants) (new-const-list domain)
  (assert (domain-p domain))
  (unless (find :constants
                (cddr domain) :key 'first)
    (error "No :constants element in this domain. Put domain in canonical form before attempting to modify the constants."))
  (let ((new-const-list
          (pddl-pprinter::minimize-canonical-type-list
           (canonicalize-types
            (pddlify-tree new-const-list)))))
  (setf
   (cdr (find :constants (cddr domain) :key 'first))
   new-const-list)))

(defun add-to-domain-constants (domain new-constants)
  (assert (domain-p domain))
  (unless (find :constants
                (cddr domain) :key 'first)
    (error "No :constants element in this domain. Put domain in canonical form before attempting to modify the constants."))
  (let ((old (pddl-pprinter::canonicalize-types (domain-constants domain)))
        (new (pddl-pprinter::canonicalize-types (pddlify-tree new-constants))))
    (iter (for (newc hyphen new-type . nil) on new by 'cdddr)
      (as pos = (position newc old))
      (assert (eq hyphen '-))
      (if (null pos)
          (appending `(,newc - ,new-type) into additions)
          (let ((old-type (nth (+ pos 2) old))
                   (old-hyp (nth (1+ pos) old))
                   (old-entry (nth pos old)))
               (assert (and old-type (eq old-hyp '-) (eq old-entry newc)))
               (unless (eq old-type new-type)
                 (error "Error, attempting to add definition of ~a of type ~a to ~a of type ~a"
                        newc new-type newc old-type))))
      (finally (setf (domain-constants domain)
                     (append old additions))))
    (domain-constants domain)))


(defun domain-requirements (domain)
  (domain-reqs domain))

(defun domain-reqs (domain)
  (assert (domain-p domain))
  (let ((head (find-if #'(lambda(e)
                           (and (listp e) (eq (car e) ':requirements)))
                       domain)))
    (rest head)))

(defun domain-types (domain)
  (assert (domain-p domain))
  (let ((head (find-if #'(lambda(e)
                           (and (listp e) (eq (car e) ':types)))
                       domain)))
    (rest head)))

(defun domain-functions (domain)
  (assert (domain-p domain))
  (let ((head (find-if #'(lambda(e)
                           (and (listp e) (eq (car e) ':functions)))
                       domain)))
    (rest head)))

(defun domain-actions (domain)
  (assert (domain-p domain))
  (remove-if-not #'(lambda (x) (eq x :action))
                 (cddr domain) :key 'first))

(defsetf domain-actions (domain) (action-list)
  `(progn
     (check-type ,domain domain)
     ;; remove all the old actions
     (setf (cddr ,domain)
           (remove :action (cddr ,domain)
                   :key #'first))
     (alexandria:appendf (cddr ,domain)
              ,action-list)))

(defun remove-domain-actions (domain)
  (assert (domain-p domain))
  `(,(pddl-symbol 'pddl:define) ,(second domain)
       ,@(remove :action (cddr domain) :key 'first)))

(defun insert-domain-actions (domain action-list)
  (assert (domain-p domain))
  (assert (every #'action-p action-list))
  ;; Assumes the ACTIONS-LIST is a list of well-formed
  ;; PDDL action objects.
  `(,(pddl-symbol 'pddl:define) ,(second domain)
       ,@(append (cddr domain) action-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions related to the components of a PDDL action.

(defmacro defaction (name params precondition effect)
  "An abbreviated version of MAKE-ACTION that allows you to
dispense with quotes and keyword arguments."
  `(make-action ',name ',params
                :precondition ',precondition
                :effect ',effect))

(defun make-action (name parameters &key precondition effect)
  (unless name (error 'type-error :datum name :type 'symbol))
  (pddlify-tree
   `(:action ,name
     :parameters ,parameters
     :precondition ,precondition
     :effect ,(if effect
                  effect
                  ;; this is bug-compatible with the current version of VAL, as of [2017/09/12:rpg]
                  '(and)))))

(defun make-durative-action (name parameters &key duration condition effect)
  (pddlify-tree
  `(:durative-action ,name
      :parameters ,parameters
      :duration ,duration
      :condition ,condition
    :effect ,effect)))

(defun action-effect (action)
  (assert (action-p action))
  (second (member ':effect action)))

(defsetf action-effect (action) (effect)
  `(progn
     (assert (action-p ,action))
     (setf
      (second
       (member :effect ,action))
      (pddlify-tree ,effect))))

(defun action-precondition (action)
  (assert (action-p action))
  (second (member ':precondition action)))

(defsetf action-precondition (action) (precond)
  `(progn
     (assert (action-p ,action))
     (setf
      (second
       (member :precondition ,action))
      (pddlify-tree ,precond))))

(defun action-params (action)
  (assert (action-p action))
  (second (member ':parameters action)))

(defsetf action-params (action) (params)
  `(progn
     (assert (action-p ,action))
     (setf
      (second
       (member :parameters ,action))
      (pddlify-tree ,params))))

(defun action-name (action)
  (assert (action-p action))
  (second action))


;; This is tailored to FastDownward. It seems there is no way
;; to define action costs in a general syntax. FD assumes a special
;; fluent defined, TOTAL-COST, and uses that to model cost. The
;; following parses an action given that, but this is not satisfactory
;; since PDDL-UTILS should not be planner-specific...
(defun action-cost (action)
  (assert (action-p action))
  (let ((effect (action-effect action))
        cost-effect)
    (setf cost-effect
          (find 'pddl::increase effect
                :key #'(lambda (predicate)
                         (and
                          (listp predicate) (listp (second predicate))
                          (eql (first (second predicate))
                                   'pddl::total-cost)
                              (first predicate)))))
    (third cost-effect)))

;; This suffers the same issue as above, being its complementary
;; function...
(defsetf action-cost (action) (cost)
  `(let ((effect (action-effect ,action))
         cost-effect
         (cost-expr (list 'pddl::increase '(pddl::total-cost) ,cost)))

     (unless effect
       (setf (action-effect ,action)
             (list 'and cost-expr)))

     (unless (eql (first effect) 'and)
       (setf effect (list 'and effect)))

     (setf cost-effect
           (find 'pddl::increase effect
                 :key #'(lambda (predicate)
                          (and
                           (listp predicate) (listp (second predicate))
                           (eql (first (second predicate))
                                'pddl::total-cost)
                           (first predicate)))))

     (if cost-effect
       (setf (third cost-effect) ,cost)
       (setf (action-effect ,action)
             (append effect (list cost-expr))))

     ,cost))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to create a well-formed PDDL expression.

(defun merge-domain-reqs (pddl-domain1 pddl-domain2)
  (remove-duplicates
   (append (domain-reqs pddl-domain1)
           (domain-reqs pddl-domain2))
   :test #'equal))

(defun merge-domain-types (pddl-domain1 pddl-domain2)
  (let ((typed-list
         (append (pddl-pprinter::canonicalize-types (domain-types pddl-domain1))
                 (pddl-pprinter::canonicalize-types (domain-types
                                                     pddl-domain2))))
        (parent-type-table (make-hash-table :test #'eql))
        all-types new-typed-list)

    (setf all-types (loop for type in typed-list
                          unless (eql type '-)
                            collect type))
    (loop
      with start = 0
      with lst = typed-list
      for pos = (position '- lst)
      as subtypes = (subseq lst start pos)
      when (null lst)
        do (return)
      when (null pos)
        do (error "Malformed typed list ~a. There are non-OBJECT types
                          without any parents." typed-list)
      do (loop for type in subtypes
               do (setf (gethash type parent-type-table)
                        (push (nth (1+ pos) lst)
                              (gethash type parent-type-table))))
      do (setf lst (subseq lst (+ pos 2))))

    (loop for type in all-types
          when (and (not (eql type 'object))
                    (null (gethash type parent-type-table)))
            do (warn "The type ~a was not declared." type)
          when (> (length (remove-duplicates
                           (gethash type
                                    parent-type-table))) 1)
            do (error "The type ~a has two parent types in the input typed list ~a."
                      type typed-list)
          when (= (length (remove-duplicates
                           (gethash type
                                    parent-type-table))) 1)
            do (setf (gethash type parent-type-table)
                     (remove-duplicates
                      (gethash type
                               parent-type-table))))

    ;; Now reconstruct...
    (maphash #'(lambda (type parent)
                 (setf new-typed-list
                       (append new-typed-list
                               `(,type - ,(first parent)))))
             parent-type-table)
    new-typed-list))


(defun merge-domain-predicates (pddl-domain1 pddl-domain2)
  (remove-duplicates
   (append (domain-predicates pddl-domain1)
           (domain-predicates pddl-domain2))
   :test #'equal)) ;; probably need a UNIFY function for this test.

(defun merge-domain-actions (pddl-domain1 pddl-domain2)
  (assert (and (domain-p pddl-domain1) (domain-p pddl-domain2)))
  (let ((result (remove-duplicates (append (domain-actions pddl-domain1)
                                           (domain-actions pddl-domain2))
                                   :test 'equalp)))
    (unless (= (length result)
               (length (remove-duplicates result :test 'eq
                                          :key 'second ; name
                                          )))
      (error "Redundantly defined actions."))
    result))

;;; FIXME: Need a test.  Suggest we introduce a sub-function that merges two typed lists.
(defun merge-problem-objects (pddl-problem1 pddl-problem2)
  "Return a merged list of objects from the two problems."
  (let ((objects1 (pddl-pprinter::canonicalize-types (problem-objects pddl-problem1)))
        (objects2 (pddl-pprinter::canonicalize-types (problem-objects pddl-problem2))))
    (pddl-pprinter::minimize-canonical-type-list (append objects1 objects2))))

(defun merge-problem-states (pddl-problem1 pddl-problem2)
  (remove-duplicates
   (append (problem-state pddl-problem1)
           (problem-state pddl-problem2))
   :test #'equal))

(defun merge-problem-goals (pddl-problem1 pddl-problem2)
  ;; This assumes a goal condition is a flat (un-nested) conjunctive
  ;; expression.
  `(and
    ,@(remove-duplicates
       (append (rest (problem-goal pddl-problem1))
               (rest (problem-goal pddl-problem2)))
       :test #'equal)))

(defun remove-types-from-list (typed-list &optional acc)
  "Return all the elements of the TYPED-LIST with
their typing information removed."
  (cond ((null typed-list) (nreverse acc))
        ((eq (first typed-list) '-)
         (remove-types-from-list (cddr typed-list) acc))
        (t (remove-types-from-list (cdr typed-list) (cons (first typed-list) acc)))))

(defun typelist-to-alist (typed-list)
  "TYPED-LIST must be a canonical-form typed list (see CANONICALIZE-TYPE-LIST).
Translates to (constant . type) alist."
  (iterate (for (constant dash type . nil) on typed-list by 'cdddr)
    (assert (eq dash '-))
    (collecting (cons constant type))))

(defun positive-literal-p (sexp &key (predicates nil predicates-supplied-p))
  (and (listp sexp)
       (not (eq (first sexp) 'not))
       (if predicates-supplied-p
           (member (first sexp) predicates)
           t)))

(defun flatten-conjunction (conj)
  "Take an s-expression and, if it is a multilayer conjunction,
make it a single-layer conjunction (intermediate AND's removed)."
  (labels ((flatten-1 (conj)
             ;; return the conj with any outside AND removed and
             ;; flattened
             (case (first conj)
               (and (alexandria:mappend #'flatten-1 (rest conj)))
               ((or forall exists imply) (error "Cannot handle a disjunction in FLATTEN-CONJUNCTION:~%~t~s" conj))
               (not (assert (= (length conj) 2))
                (if (positive-literal-p (second conj))
                    conj
                    (error "Cannot handle negations other than negated literals in flatten-conjunction: ~s"
                           conj)))
               (otherwise (list conj)))))
    (if (eq (first conj) 'and)
        `(and ,@ (flatten-1 conj))
        (flatten-1 conj))))
