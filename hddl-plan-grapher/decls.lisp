(in-package #:hddl-plan-grapher)

(defclass hddl-plan-tree-graph ()
  ((node-lookup-table
    :initform (make-hash-table :test 'eql) ; node keys are integers.
    :reader node-lookup-table
    ))
  (:documentation "A null class that the user may subclass to
tailor display of HDDL plan trees."))

(defclass has-task ()
  ((task                                ; s-expression
    :initarg :task
    :reader task
    )))

(defclass action (has-task)
  ()
  )

(defclass decomposition (has-task)
  ((method-name
    :initarg :method-name
    :reader method-name
    )
   (children                            ; list of integers
    :initarg :children
    :reader children
    ))
  )



(defgeneric graph-plan-tree (plan-tree &key attributes
                                         graph-object))
