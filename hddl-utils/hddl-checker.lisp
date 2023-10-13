(in-package :hddl-utils)

(deftype only-values (&rest value-spec)
  `(values ,@value-spec &optional))

(deftype only-value (value-spec)
  `(values ,value-spec &optional))

(defclass domain-info ()
  ((name-types
    :initarg :name-types
    :reader name-types
    :type hash-table
    :initform (make-hash-table :test 'eq)
    :documentation "A table mapping names in the domain to types of entity,
including :task, :method, :action, :type, :predicate, :object"
    )
   (name-arities
    :initarg :name-arities
    :reader name-arities
    :type hash-table
    :initform (make-hash-table :test 'eq)
    ))
  )

;;; FIXME: rename to HDDL-flaw
(define-condition domain-flaw ()
  ((context
    :initarg :context
    :reader context
    :initform nil
    :type (or null string)
    )))


(define-condition inconsistent-name-type (domain-flaw)
  ((name
    :initarg :name
    :reader name
    )
   (prev-type
    :initarg :prev-type
    :reader prev-type
    )
   (new-type
    :initarg :new-type
    :reader new-type
    ))
  (:report (lambda (c s)
             (format s "Inconsistent name type: ~s first defined as ~s is redefined as ~s~@[ in ~a~]."
                     (name c) (prev-type c) (new-type c) (context-string (context c))))))

(define-condition inconsistent-name-arity (domain-flaw)
  ((name
    :initarg :name
    :reader name
    )
   (prev-arity
    :initarg :prev-arity
    :reader prev-arity
    )
   (new-arity
    :initarg :new-arity
    :reader new-arity
    ))
  (:report (lambda (c s)
             (format s "Inconsistent name arity: ~s first defined as arity ~d is redefined as ~d~@[ in ~a~]."
                     (name c) (prev-arity c) (new-arity c) (context-string (context c))))))

(define-condition incorrect-arity (domain-flaw)
  ((name
    :initarg :name
    )
   (arity
    :initarg :arity
    )
   (expr
    :initarg :expr
    ))
  (:report (lambda (c s)
             (with-slots (name arity expr context) c
               (format s "Predicate ~a used with incorrect arity ~a, should be ~d~@[ in ~a~]."
                       name expr arity (context-string context))))))

(define-condition undefined-task (domain-flaw)
  ((task-name
    :initarg :task-name
    )
   (type
    :initform "task"
    :initarg :type))
  (:report (lambda (c s)
             (with-slots (task-name type context) c
              (format s "Undefined ~a name ~s~@[ in ~a~]."
                      type task-name (context-string context))))))

(define-condition undefined-subtask (undefined-task)
  ()
  (:default-initargs :type "subtask (task or action)"))

(define-condition undefined-type (domain-flaw)
  ((type-name
    :initarg :type-name
    ))
  (:report (lambda (c s)
             (with-slots (type-name context) c
               (format s "Undefined type name ~s~@[ in ~a~]." type-name
                       (context-string context))))))

(define-condition bad-variable-name (domain-flaw)
  ((name
    :initarg :name
    ))
  (:report (lambda (c s)
             (with-slots (name context) c
              (format s "Incorrect variable name ~a~@[ in ~a~]." name
                      (context-string context))))))

;;;---------------------------------------------------------------------------
;;; Setter macros
;;;---------------------------------------------------------------------------


(defmacro set-name-arity ((domain-info name &optional context) arity)
  `(set-name-arity-fun ,domain-info ,name ,context ,arity))

(defmacro set-name-type ((domain-info name &optional context) type)
  `(set-name-type-fun ,domain-info ,name ,context ,type))

;;; FIXME: for later refinement
(defun context-string (context)
  (when context
    (format nil "~a" context)))

(defun print-domain-info (domain-info &optional (stream t))
  (format stream "~&Name types:~%")
  (let* ((names (sort (alexandria:hash-table-keys (name-types domain-info)) #'string-lessp))
         (max-name-length
           (reduce #'max (mapcar #'symbol-name names) :key 'length :initial-value 0))
         (format-string (format nil "~~&~~~da   ~~a~~%" max-name-length)))
    (iter (for sym in names)
      (format stream format-string sym (name-type domain-info sym))))
  (format stream "~&Name arities:~%")
  (let* ((names (alexandria:hash-table-keys (name-arities domain-info)))
         (max-name-length
           (reduce #'max (mapcar #'symbol-name names) :key 'length :initial-value 0))
         (max-arity (reduce #'max (alexandria:hash-table-values (name-arities domain-info)) :initial-value 0))
         (arity-width (1+ (floor max-arity 10)))
         (format-string (format nil "~~&~~~da   ~~~da~~%" max-name-length arity-width)))
    (iter (for sym in names)
      (format stream format-string sym (name-arity domain-info sym)))))

(defun check-hddl-domain-file (domain-file)
  (let ((domain (read-hddl-file domain-file)))
    (check-domain domain)))

(defun check-hddl-domain-and-problem-files (domain-file problem-file)
  (let ((problem (read-hddl-file problem-file)))
    (multiple-value-bind (success domain-info)
        (check-hddl-domain-file domain-file)
      (if success
          (check-problem problem domain-info)
          (progn
            (format t "~&Stopped before checking problem because of failure checking domain.~%")
            (values nil domain-info))))))


(defun check-domain (domain &optional verbose)
  (let ((domain-info (make-instance 'domain-info))
        domain-flaws)
    (set-name-type (domain-info 'hddl::object) :type)
    (handler-bind ((domain-flaw #'(lambda (c)
                                    (push c domain-flaws)
                                    (continue c))))
      (check-types domain-info (canonicalize-types (domain-types domain)))
      ;; at this point all types should be defined...
      (check-constants domain-info (canonicalize-types (domain-constants domain)))
      (check-predicates domain-info (domain-predicates domain))
      (check-tasks domain-info (domain-tasks domain))
      (check-actions domain-info (domain-actions domain))
      (check-methods domain-info (domain-methods domain)))
    (when verbose (print-domain-info domain-info))
    (values
     (if domain-flaws
         (progn
           (print-domain-flaws domain-flaws)
           nil)
         t)
     domain-info)))

(defun check-problem (problem domain-info &optional verbose)
   (let (problem-flaws)
    (handler-bind ((domain-flaw #'(lambda (c)
                                    (push c problem-flaws)
                                    (continue c))))
      ;; at this point all types should be defined...
      (check-objects domain-info (canonicalize-types (problem-objects problem)))
      (check-initial-state domain-info (problem-state problem))
      ;(check-htn (problem-htn problem))
      )
    (when verbose (print-domain-info domain-info))
    (values
     (if problem-flaws
         (progn
           (print-domain-flaws problem-flaws)
           nil)
         t)
     domain-info))
  )

(defun print-domain-flaws (flaw-list)
  (iter (for flaw in flaw-list)
    (princ flaw *error-output*)
    (terpri *error-output*)))

;;; PRECONDITION: must be called *after* CHECK-TYPES
(defun check-constants (domain-info constants)
  (iter (for (name hyphen type . rest) on constants by 'cdddr)
    (declare (ignorable rest))
    (unless (eq hyphen '-)
      (error "Ill-formed constant definition: ~a" `(,name ,hyphen ,type)))
    (set-name-type (domain-info name "constant definitions") :object)
    (verify-type-name domain-info type "constant definitions")))

(defun check-objects (domain-info objects)
  (iter (for (name hyphen type . rest) on objects by 'cdddr)
    (declare (ignorable rest))
    (unless (eq hyphen '-)
      (error "Ill-formed constant definition: ~a" `(,name ,hyphen ,type)))
    (set-name-type (domain-info name "problem object definitions") :object)
    (verify-type-name domain-info type "problem object definitions")))

(defun check-initial-state (domain-info facts)
  (iter (for fact in facts)
    (as pred = (first fact))
    (verify-predicate-name domain-info pred)
    (let ((arity (name-arity domain-info pred)))
      (unless (= arity (length (rest fact)))
        (signal 'incorrect-arity :arity arity :name pred :expr fact)))))

(defun check-predicates (domain-info predicates)
  (iter (for (name . parameters) in predicates)
    (as arity = (check-parameter-list domain-info parameters
                                      (format nil "parameter spec for definition of predicate ~a" name)))
    (set-name-type (domain-info name "predicate definitions") :predicate)
    (set-name-arity (domain-info name "predicate definitions") arity)))

(defun check-parameter-list (domain-info parameters &optional context)
  "Check that PARAMETERS is a well-formed parameter list, signaling flaws as appropriate.
Return the arity of the parameter list."
  (iter (for (var hyphen type . rest) on (canonicalize-types parameters) by 'cdddr)
    (with arity = 0)
    (declare (ignorable rest))
    (unless (eq hyphen '-)
      (error "Ill-formed type definition: ~a" `(,var ,hyphen ,type)))
    (unless (typep var 'hddl-variable)
      (signal 'bad-variable-name :name var :context context))
    (verify-type-name domain-info type context)
    (incf arity)
    (finally (return arity))))

(defun check-tasks (domain-info tasks)
  (iter (for task in tasks)
    (as name = (second task))
    (as context = (format nil "parameter spec for definition of task ~a" name))
    (as arity = (check-parameter-list domain-info (task-parameters task) context))
    (assert (eq (first task) :task))
    (set-name-type (domain-info name "task definitions") :task)
(set-name-arity (domain-info name context) arity)))

(defun check-methods (domain-info methods)
  (iter (for method in methods)
    (as keyword = (first method))
    (as name = (second method))
    (as method-task = (method-task method))
    (as task-name = (first method-task))
    (assert (eq keyword :method))
    (verify-task-name domain-info task-name (format nil "method task for definition of method ~a" name))
    (set-name-type (domain-info name (format nil "method definition of method named ~a" name)) :method)
    (check-method-subtasks domain-info
                           ;; this way puts subtasks in flattened-conj form and then takes the list of subtasks
                           (rest (flatten-conjunction (method-subtasks method) nil))
                           (format nil "Subtasks of method ~a" name))))

(defun check-method-subtasks (domain-info subtasks &optional context)
  (iter (for subtask in subtasks)
    (as task-name = (first subtask))
    (as valid-task = (verify-subtask-name domain-info task-name))
    (when valid-task
     (let ((arity (name-arity domain-info task-name)))
       (unless (= arity (length (rest subtask)))
         (signal 'incorrect-arity :arity arity :name task-name :expr subtask :context context))))))

(defun verify-type-name (domain-info type-name &optional context)
  (let ((name-type (name-type domain-info type-name)))
    (cond ((null name-type)
           (signal 'undefined-type :type-name type-name :context context))
          ((not (eq name-type :type))
           (signal 'inconsistent-name-type :new-type :type :prev-type name-type
                                           :name type-name :context context)))))

(defun verify-task-name (domain-info task-name &optional context)
  (let ((name-type (name-type domain-info task-name)))
    (cond ((null name-type)
           (signal 'undefined-task :task-name task-name :context context)
           nil)
          ((not (eq name-type :task))
           (signal 'inconsistent-name-type :new-type :task :prev-type name-type
                                           :name task-name :context context)
           nil)
          (t t))))

(defun verify-subtask-name (domain-info task-name &optional context)
  (let ((name-type (name-type domain-info task-name)))
    (cond ((null name-type)
           (signal 'undefined-subtask :task-name task-name :context context)
           nil)
          ((or (eq name-type :action) (eq name-type :task))
           t)
          (t
           (signal 'inconsistent-name-type :new-type :subtask :prev-type name-type
                                           :name task-name :context context)
           nil))))

(defun verify-predicate-name (domain-info predicate-name &optional context)
  (let ((name-type (name-type domain-info predicate-name)))
    (cond ((null name-type)
           (signal 'undefined-predicate :predicate-name predicate-name :context context))
          ((not (eq name-type :predicate))
           (signal 'inconsistent-name-type :new-type :type :prev-type name-type
                                           :name predicate-name :context context)))))


(defun check-actions (domain-info actions)
  (iter (for action in actions)
  (as keyword = (first action))
  (as name = (second action))
  (as context = (format nil "definition of action ~a" name))
  (assert (eq keyword :action))
  (set-name-type (domain-info name context) :action)
  (let ((arity (check-parameter-list domain-info (action-params action)
                                       context)))
    (set-name-arity (domain-info name context) arity))))

(defun check-types (domain-info types)
  (let ((context "type definition"))
    (iter (for (type hyphen super-type . rest) on types by 'cdddr)
      (with super-types)
      (declare (ignorable rest))
      (unless (eq hyphen '-)
        (error "Ill-formed type definition: ~a" `(,type ,hyphen ,super-type)))
      (set-name-type (domain-info type context) :type)
      (pushnew super-type super-types)
      (finally (mapc #'(lambda (super-type) (verify-type-name domain-info super-type "type definition supertypes")) super-types)))))



(declaim (ftype (function (domain-info symbol)
                          (only-value symbol))
                name-type))
(defun name-type (domain-info name)
  (the symbol (gethash name (name-types domain-info))))

(defun set-name-type-fun (domain-info name context type)
  (with-slots (name-types) domain-info
    (if-let ((prev-type (the symbol (name-type domain-info name))))
      (unless (eq type prev-type)
        (restart-case
            (signal 'inconsistent-name-type
                    :name name :prev-type prev-type :new-type type)
          (continue () prev-type)
          (abort (&optional c)
            (error "~a" (if c c
                            (make-instance 'inconsistent-name-type
                                           :context context
                                           :name name :prev-type prev-type :new-type type))))))
      (setf (gethash name (name-types domain-info)) type))))

(declaim (ftype (function (domain-info symbol)
                          (only-value (integer 0)))
                name-arity))
(defun name-arity (domain-info name)
  (the (or null (integer 0)) (gethash name (name-arities domain-info))))

(defun set-name-arity-fun (domain-info name context arity)
  (with-slots (name-types) domain-info
    (if-let ((prev-arity (name-arity domain-info name)))
      (unless (eql arity prev-arity)
        (restart-case
            (signal 'inconsistent-name-arity
                    :name name :prev-arity prev-arity :new-arity arity
                    :context context)
          (continue () prev-arity)
          (abort (&optional c)
            (error "~a" (if c c
                            (make-instance 'inconsistent-name-arity
                                           :context context
                                           :name name :prev-arity prev-arity :new-arity arity))))))
      (setf (gethash name (name-arities domain-info)) arity))))
