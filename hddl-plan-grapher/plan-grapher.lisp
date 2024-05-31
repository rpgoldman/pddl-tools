(in-package #:hddl-plan-grapher)

(defmethod graph-plan-tree ((plan-tree-file string)
                            &key (attributes nil)
                              (graph-object (make-instance 'hddl-plan-tree-graph)))
  (let ((pathname (merge-pathnames (parse-namestring plan-tree-file)
                                   (make-pathname :type "hddl"))))
    (graph-plan-tree pathname
                     :attributes attributes
                     :graph-object graph-object)))

(defmethod graph-plan-tree ((plan-tree-file pathname)
                            &key (attributes nil)
                                  (graph-object (make-instance 'hddl-plan-tree-graph)))
  (let ((plan-tree (hddl-io:read-hddl-plan-file plan-tree-file)))
    (unless (eq (first plan-tree) ':hddl-plan)
      (error 'type-error :datum plan-tree :expected-type 'hddl-plan))
    (graph-plan-tree plan-tree
                     :attributes attributes
                     :graph-object graph-object)))

(defmethod graph-plan-tree (plan-tree &key (attributes nil)
                                        (graph-object (make-instance 'hddl-plan-tree-graph)))
  "Takes a SHOP plan forest (PLAN-FOREST) as input, and returns a CL-DOT graph object."
  (let ((roots (getf (rest plan-tree) :roots))
        (actions (getf (rest plan-tree) :actions))
        (decompositions (getf (rest plan-tree) :decompositions)))
    (build-lookup-table graph-object actions decompositions)
    (cl-dot:generate-graph-from-roots graph-object roots attributes)))

(defmethod build-lookup-table ((graph-object hddl-plan-tree-graph) actions decompositions)
  (iter (for (index . task) in actions)
    (setf (gethash index (node-lookup-table graph-object))
          (make-instance 'action :task task)))
  (iter (for (index task method-name . children) in decompositions)
    (setf (gethash index (node-lookup-table graph-object))
          (make-instance 'decomposition :task task
                         :method-name method-name
                         :children children))))

(defmethod lookup ((g hddl-plan-tree-graph) (index integer))
  (or (gethash index (node-lookup-table g))
      (error "No graph node with index ~d" index)))

(defmethod cl-dot:graph-object-node ((g hddl-plan-tree-graph) (index integer))
  (cl-dot:graph-object-node g (lookup g index)))

(defmethod cl-dot:graph-object-node ((g hddl-plan-tree-graph) (obj action))
  (declare (ignorable g))
  (make-instance 'cl-dot:node
    :attributes `(:label ,(format nil "~A" (task obj))
                         :shape :box)))

(defmethod cl-dot:graph-object-node ((g hddl-plan-tree-graph) (obj decomposition))
  (declare (ignorable g))
  (make-instance 'cl-dot:node
    :attributes `(:label ,(format nil "~A" (task obj))
                  :style :rounded
                  :shape :box)))

(defmethod cl-dot:graph-object-points-to ((g hddl-plan-tree-graph) (index integer))
  (cl-dot:graph-object-points-to g (lookup g index)))

(defmethod cl-dot:graph-object-points-to ((g hddl-plan-tree-graph)(obj action))
  (declare (ignorable g obj))
  nil)


(defmethod cl-dot:graph-object-points-to ((g hddl-plan-tree-graph)(obj decomposition))
  (declare (ignorable g))
  (children obj))
