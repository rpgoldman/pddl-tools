;;; -------------------------------------------------------------------------
;;; Copyright 2023, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(in-package :hddl-utils)

;; makers
(defun make-domain (name &key requirements constants predicates actions
                    types tasks methods)
  (let ((constants (hddlify-tree constants))
        (predicates (hddlify-tree predicates))
        (actions (hddlify-tree actions))
        (types (hddlify-tree types))
        (tasks (hddlify-tree tasks))
        (methods (hddlify-tree methods)))
    (if (member :durative-actions requirements)
        (assert (every #'durative-action-sexp-p actions))
        (assert (every #'action-sexp-p actions)))
    `(,(pddl-symbol 'pddl:define) (,(pddl-symbol 'pddl:domain) ,(hddlify name))
         (:requirements ,@requirements)
       (:types ,@types)
       (:constants ,@constants)
       (:predicates ,@predicates)
      ,@tasks
      ,@methods
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
        (methods (mapcar #'canonicalize-method (domain-methods old-domain)))
        (tasks (domain-tasks old-domain))
        (actions (domain-actions old-domain)))
    (make-domain (domain-name old-domain)
                 :requirements requirements
                 :types types
                 :constants constants
                 :predicates predicates
                 :tasks tasks
                 :methods methods
                 :actions actions)))

(defun canonicalize-method (original)
  (unless (or (find :ordered-subtasks original)
              (find :ordered-tasks original))
    (error "CANONICALIZE-METHOD does not yet support partially-ordered methods."))
  (make-ordered-method (method-name original)
                       (method-task original)
                       (method-parameters original)
                       :precond (method-precondition original)
                       :tasks (let ((tasks (method-subtasks original)))
                                (if (eq (first tasks) 'and)
                                    (rest tasks)
                                    tasks))))

(defun make-problem (name &key requirements domain objects init goal
                            htn
                            (complete-p t))
  "Make a new PDDL problem s-expression initialized as per the keyword
arguments.  Unless COMPLETE-P is NIL, will check for mandatory components."
  (when complete-p
    (unless domain (error "DOMAIN argument is mandatory."))
    ;; (unless goal (error "GOAL argument is mandatory."))
    )
  (flet ((negated (fact) (eq (first fact) 'not)))
    (let ((domain (hddl-symbol domain))
          (objects (hddlify-tree objects))
          (htn (hddlify-tree htn))
          (init (hddlify-tree init))
          (goal (hddlify-tree goal)))
      (when (some #'negated init)
        (cerror "Remove negated initial facts."
                "Negated facts in :init are unnecessary and may break some planners.")
        (setf init (remove-if #'negated init)))
      (unless (= (length init) (length (remove-duplicates init :test 'equal)))
        (cerror "Remove duplicates"
                "Some duplicated facts in init.  This is known to break some planners.")
        (setf init (remove-duplicates init :test 'equal)))
      `(,(pddl-symbol 'pddl:define) (,(pddl-symbol 'pddl:problem) ,(hddl-symbol name))
        (:domain ,domain)
        ,@(when requirements
            `((:requirements ,@requirements)))
        (:objects ,@objects)
        (:init ,@init)
        (:htn ,@htn)
        (:goal ,goal)))))

(defun canonicalize-problem (problem)
  (make-problem (problem-name problem)
                :requirements (problem-requirements problem)
                :domain (problem-domain problem)
                :objects (problem-objects problem)
                :init (problem-state problem)
                :goal (problem-goal problem)
                :htn (problem-htn problem)))


;;; misc utility

(defun make-complex-task (task-name params)
  `(:task ,task-name :parameters ,(copy-tree params)))

(defun task-name (complex-task)
  (getf complex-task :task))

(defun task-parameters (complex-task)
  (getf complex-task :parameters))

(defun predicate-name (pred-def)
  (first pred-def))

(defun predicate-parameters (pred-def)
  (rest pred-def))


;;;---------------------------------------------------------------------------
;;; Types
;;;---------------------------------------------------------------------------

(defun method-p (sexp)
  (eq (first sexp) :method))

(deftype method-def ()
  '(satisfies method-p))

(defun ordered-method-p (method)
  (and (typep method 'method-def)
       ;; the first element of METHOD should be :METHOD and the
       ;; second the method-name, so we start looking at the CDDR.
       (or (find :ordered-subtasks (cddr method))
           (find :ordered-tasks (cddr method)))))

(deftype ordered-method-def ()
  '(satisfies ordered-method-p))


(defun task-p (sexp)
  (eq (first sexp) :task))

(deftype task-def ()
  '(satisfies task-p))


(defun make-ordered-method (method-name task-sexpr params &key precond tasks)
  `(:method ,method-name :parameters ,(copy-tree params)
     :task ,task-sexpr
     :precondition ,(copy-tree precond)
     :ordered-subtasks ,(flatten-conjunction (copy-tree tasks) nil)))

(defun method-subtasks (method)
  (cond ((find :ordered-subtasks method)
         (getf method :ordered-subtasks))
        ((find :sub-tasks method)
         (getf method :sub-tasks))
        ((find :tasks method)
         (getf method :tasks))
        (t (error "Unable to find subtasks in method definition:~%~s" method))))

(defsetf method-subtasks (method) (subtasks)
  (let ((subtasks-var (gensym "SUBTASKS")))
    `(let ((,subtasks-var (when ,subtasks      ; nil is just nil...
                       (flatten-conjunction (hddlify-tree ,subtasks) nil))))
       (cond ((getf ,method :ordered-subtasks nil)
              (setf (getf ,method :ordered-subtasks) ,subtasks-var))
             ((getf ,method :tasks nil)
              (setf (getf ,method :tasks) ,subtasks-var))
             (t (setf (getf ,method :sub-tasks) ,subtasks-var))))))

(defun method-task (method)
  (getf method :task))

(defsetf method-task (method) (task)
  `(setf (getf ,method :task) ,task))

(defun method-name (method)
  (second method))

(defsetf method-name (method) (task)
  `(setf (second ,method) ,task))

(defun method-task-net (method)
  (if (ordered-method-p method)
      (method-subtasks method)
      (error "Do not yet support partially-ordered task networks.")))

(defun method-parameters (method)
  (getf method :parameters))

(defsetf method-parameters (method) (parameters)
  `(setf (getf ,method :parameters) ,parameters))

(defun method-precondition (method)
  (getf method :precondition))

(defsetf method-precondition (method) (precondition)
  `(setf (getf ,method :precondition) ,precondition))


(defsetf problem-domain (problem) (domain)
  `(let ((*pddl-package* *hddl-package*))
     (assert (pddl-utils::problem-p ,problem))
     (setf (pddl-utils:problem-domain ,problem) ,domain)))

(defun problem-domain (problem)
  (pddl-utils:problem-domain problem))

(defun problem-htn (problem)
  (problem-element problem :htn))

(defsetf problem-htn (problem) (&rest htn)
  `(let ((*pddl-package* hddl-io::*hddl-package*))

     (setf (pddl-utils:problem-element ,problem :htn)
           (hddlify-tree ,htn))))


(defun domain-tasks (domain)
  (check-type domain domain)
  (remove-if-not #'(lambda (x) (eq x :task))
                 (cddr domain) :key 'first))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defparameter +ordered-subtask-keywords+
   '(:ordered-subtasks :ordered-tasks))

 (defparameter +subtask-keywords+
   (append +ordered-subtask-keywords+
           '(:subtasks :tasks))))

(defun ordered-task-net-p (task-net)
  (and (listp task-net)
       (some #'(lambda (key) (find key task-net)) +ordered-subtask-keywords+)))

(defun p-htn-p (x)
  "Is the s-expression X a P-HTN (problem-htn)?"
  (and (listp x)
       (some #'(lambda (key) (find key x)) +subtask-keywords+)))

(deftype p-htn ()
  '(satisfies p-htn-p))

(defun p-htn-parameters (htn)
  (getf htn :parameters))

(defun p-htn-ordered-subtasks (htn)
  (or
   (iter (for key in +ordered-subtask-keywords+)
     (alexandria:when-let ((subtasks
                            (getf htn key)))
       (return subtasks))
     (finally (return nil)))
   (error "No ordered subtasks found in HTN: ~s" htn)))

(deftype hddl-variable ()
  'pddl-variable)


;;; helper function
;;; allows us to match a tag against x as the first component,
;;; ignoring elements that are not lists.
(defun tag-key (x)
  (when (listp x) (first x)))


;;; The grammar
;;; 1 <domain> ::= (define (domain <name>)
;;; 2          [<require-def>]
;;; 3          [<types-def>]:typing
;;; 4          [<constants-def>]
;;; 5          [<predicates-def>]
;;; 6          <comp-task-def>*
;;; 7          <method-def>*
;;; 8          <action-def>*)
;;; So we need to find out what comes immediately before
;;; and immediately after where the tasks are to go.
(defsetf domain-tasks (domain) (tasks)
  `(progn
     (check-type ,domain domain)
     (let ((tasks-start (or (position :task ,domain :key #'tag-key)
                             (1+ (or
                                  (position :predicates ,domain :key #'tag-key :from-end t)
                                  (position :constants ,domain :key #'tag-key :from-end t)
                                  (position :types ,domain :key #'tag-key :from-end t)
                                  (position :requirements ,domain :key #'tag-key :from-end t)
                                  1))))
           (after-tasks (or (position :method ,domain :key #'tag-key)
                            (position :action ,domain :key #'tag-key)
                            (length ,domain))))
       (setf ,domain
             (splice ,domain :start tasks-start :end after-tasks :new ,tasks))
       (domain-tasks ,domain))))


(defun remove-domain-tasks (domain)
  (check-type domain domain)
  `(,(pddl-symbol 'pddl:define) ,(second domain)
       ,@(remove :task (cddr domain) :key 'first)))

(defun before-tasks-pos (domain)
  ;; the first two positions are for the define keyword and the domain name
  ;; the tasks go after the last of predicates, constants, types, requirements
  (flet ((constituent-pos (keyword)
           (position keyword domain :from-end t :key #'(lambda (x)
                                                         (when (listp x) (car x))))))
    (or (constituent-pos :predicates)
                 (constituent-pos :constants)
                 (constituent-pos :types)
                 (constituent-pos :requirements)
                 1)))

(defun insert-domain-task (domain task-def)
  (check-type domain domain)
  (assert (complex-task-sexp-p task-def))
  (let ((last-before (before-tasks-pos domain)))
    (append
     (subseq domain 0 (1+ last-before))
     (list task-def)
     (subseq domain (1+ last-before)))))

(defun insert-domain-tasks (domain task-list)
  (check-type domain domain)
  (assert (every #'complex-task-sexp-p task-list))
  (let ((last-before (before-tasks-pos domain)))
    (append
     (subseq domain 0 (1+ last-before))
     (copy-list task-list)
     (subseq domain (1+ last-before)))))

(defsetf problem-goal (problem) (goal)
  `(let ((*pddl-package* hddl-io::*hddl-package*))
     (setf (pddl-utils:problem-goal ,problem) ,goal)))

(defun problem-goal (problem)
  (pddl-utils:problem-goal problem))

(defsetf problem-state (problem) (state)
  `(let ((*pddl-package* hddl-io::*hddl-package*))
     (setf (pddl-utils:problem-state ,problem) ,state)))

(defun problem-state (problem)
  (pddl-utils:problem-state problem))

(defun domain-methods (domain)
  (check-type domain domain)
  (remove-if-not #'(lambda (x) (eq x :method))
                 (cddr domain) :key 'first))


;;; from Edi Weitz's Common Lisp Recipes
(defun splice (list &key (start 0) (end (length list)) new)
  "Destructively modifies LIST by replacing the values START (inclusive) to
END (exclusive) with NEW.
  START is the index (zero-based) of the first argument to be replaced
\(e.g., 1 means the first element remains), and END is the first
element to *remain*.
  Returns the (modified) list."
  (setf list (cons nil list))
  (let ((reroute-start (nthcdr start list)))
    (setf (cdr reroute-start)
          (nconc (make-list (length new))
                 (nthcdr (- end start)
                         (cdr reroute-start)))
          list (cdr list)))
  (replace list new :start1 start)
  list)

(defsetf domain-methods (domain) (methods)
  (let ((method-var (gensym "METHODS")))
   `(progn
      (check-type ,domain domain)
      (let* ((method-tail (position :method ,domain :key #'tag-key))
             (method-tail-end (position :method ,domain
                                        :from-end t
                                        :key #'tag-key))
             (,method-var (copy-tree ,methods))
             action-tail)
        (cond (method-tail
               (setf ,domain
                     (splice ,domain :start method-tail :end (1+ method-tail-end) :new ,method-var)))
              ((setf action-tail
                     (position :action ,domain :key #'tag-key))
               (setf ,domain
                     (splice ,domain :start action-tail :end action-tail :new ,method-var)))
              (t (setf (cdr (last ,domain)) ,method-var)))
        ;; return something that fits with what SETF should return
        (domain-methods ,domain)))))

(defun remove-domain-methods (domain)
  (check-type domain domain)
  `(,(pddl-symbol 'pddl:define)
    ,@(remove :method (rest domain) :key 'first)))

(defun insert-domain-method (domain method-def)
  (check-type domain domain)
  ;;(assert (method-def-p method-def))
  ;; Assumes the ACTIONS-LIST is a list of well-formed
  ;; PDDL action objects.
  `(,(pddl-symbol 'pddl:define) ,(second domain)
    ,@(append (cddr domain) (list method-def))))

(defun insert-domain-methods (domain method-list)
  (check-type domain domain)
  (let ((new-domain (copy-tree domain))
        (old-methods (domain-methods domain)))
    (setf (domain-methods new-domain)
          (append old-methods method-list))
    new-domain))

;;; getter is the same
(defun domain-actions (domain)
  (pddl-utils:domain-actions domain))

(defsetf domain-actions (domain) (actions)
  `(progn
    (check-type ,domain domain)
    (let ((action-tail (position :action ,domain :key #'tag-key)))
      (if (null action-tail)
          (setf (cdr (last ,domain)) ,actions)
          ;; else there are actions that must be removed
          (setf (cdr (last (nbutlast ,domain (- (length ,domain) action-tail)))) ,actions))
      ;; return something that fits with what SETF should return
      (domain-actions ,domain))))

(defun domain-predicates (domain)
  (pddl-utils:domain-predicates domain))

(defsetf domain-predicates (domain) (new-pred-list)
  `(let ((*pddl-package* *hddl-package*))
     (setf (pddl-utils:domain-predicates ,domain) ,new-pred-list)))

(defun hddl-plan-to-pddl-plan (hddl-plan)
  "Take the S-expression form of an HDDL plan and return a PDDL plan extracted from it."
  (pddlify-tree
   (mapcar #'cdr (getf hddl-plan :actions))))

(defun hddl-domain-to-pddl-domain (hddl-domain)
  (pddl-utils:make-domain (domain-name hddl-domain)
                          :requirements (remove :hierarchy
                                                (domain-requirements hddl-domain))
                          :constants (domain-constants hddl-domain)
                          :predicates (domain-predicates hddl-domain)
                          :actions (domain-actions hddl-domain)
                          :types (domain-types hddl-domain)))

(defun hddl-problem-to-pddl-problem (hddl-problem)
  (pddl-utils:make-problem (problem-name hddl-problem)
                           :domain (problem-domain hddl-problem)
                          :requirements (remove :hierarchy
                                                (problem-requirements hddl-problem))
                          :objects (problem-objects hddl-problem)
                          :init (problem-state hddl-problem)
                          :goal (problem-goal hddl-problem)))
