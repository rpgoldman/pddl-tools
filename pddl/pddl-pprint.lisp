;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(in-package :pddl-pprinter)

(defvar *pddl-package* :pddl)
(defvar *is-problem* nil)
(defvar *is-domain* nil)
(defvar *canonical*
  nil
  "Print typed lists in CANONICAL form instead of MINIMAL form.")

(defparameter *pddl-pprint-dispatch* (copy-pprint-dispatch))

;;;---------------------------------------------------------------------------
;;; Interface functions
;;;---------------------------------------------------------------------------

(locally (declare #+sbcl (sb-ext:muffle-conditions style-warning))
  (defun pprint-pddl (sexp &optional (stream t) &key (canonical nil))
    "Pretty-print a PDDL plan or domain.  Carefully crafted to
generate PDDL that the FF PDDL parser will read.
  If the CANONICAL keyword argument is T, then typed lists will be
printed in \"canonical form\", as pairs like:
FOO - TYPE1 BAR - TYPE1 BAZ - TYPE2
instead of \"minimal form\" like
FOO BAR - TYPE1 BAZ - TYPE2."
    (let ((*print-pprint-dispatch* *pddl-pprint-dispatch*)
          (*package* (find-package *pddl-package*))
          (*canonical* canonical)
          *is-problem* *is-domain*)
      (cond ((domain-p sexp) (setf *is-domain* t))
            ((problem-p sexp) (setf *is-problem* t))
            (t (error "Can't determine if argument is problem or domain.")))
      (pprint sexp stream))))

(defun read-pddl-file (dom-prob-file)
  "Takes a domain or problem file and returns its s-expression."
  (let ((*package* (find-package *pddl-package*)))
    (with-open-file (dom-prob dom-prob-file :direction :input)
      (read dom-prob nil nil))))

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

(defun read-pddl-plan-file (filename)
  "Read a PDDL plan from FILENAME and return it
in the form of a list of actions."
  (with-open-file (str filename)
    (read-pddl-plan str)))

(defun read-pddl-plan (stream)
  "Read a PDDL plan from STREAM and return it
in the form of a list of actions."
  (loop for x =   (let ((*package* (find-package *pddl-package*)))
                    (read stream nil nil))
        while x
        collect x))

 


;;;---------------------------------------------------------------------------
;;; Type checkers
;;;---------------------------------------------------------------------------
(declaim (inline pddl-symbol))
(defun pddl-symbol (symbol &optional (package *pddl-package*))
  (let ((symbol (etypecase symbol
                  (string (string-upcase (string-left-trim (list #\space #\tab) symbol)))
                  (symbol symbol))))
   (uiop:intern* symbol package)))

(defun define-keyword ()
  (pddl-symbol '#:define))
(defun domain-keyword ()
  (pddl-symbol '#:domain))
(defun problem-keyword ()
  (pddl-symbol '#:problem))


(defun domain-p (pddl)
  (and (listp pddl)
       (eq (first pddl) (define-keyword))
       (eql (first (second pddl)) (domain-keyword))))

(defun problem-p (pddl)
  (and (listp pddl)
       (eq (first pddl) (define-keyword))
       (eql (first (second pddl)) (problem-keyword))))

(defun toplevel-p (pddl)
  (and (listp pddl)
       (eq (first pddl) (define-keyword))))

(defun domain-spec-p (lst)
  (and (listp lst)
       (eq (first lst) (domain-keyword))))

(defun typed-list-p (lst)
  "True if LST is a list of variables, optionally with types."
  (iter (with l = lst)
    (while l)
    (let* ((dp (position '- l))
           (vars (subseq l 0 dp))
           (type (when dp (nth (1+ dp) l))))
      ;; everything before the hyphen (if there is one)
      ;; must be a PDDL variable name
      (unless
        (every #'pddl-var-p vars)
        (return nil))
      (when type
        (unless (symbolp type)
          (return nil)))
      (setf l
            (when dp
              (subseq l (+ dp 2))))
      (finally (return t)))))

(defun pddl-var-p (x)
  (and x (symbolp x)
       (eql #\? (aref (symbol-name x) 0))))

;;;---------------------------------------------------------------------------
;;; Pretty-print table entries
;;;---------------------------------------------------------------------------

;;; NIL isn't acceptable PDDL for ().
(set-pprint-dispatch 'null
                     #'(lambda (str obj)
                         (declare (ignore obj))
                         (format str "()"))
                     0
                     *pddl-pprint-dispatch*)

(defun toplevel-printer (str obj)
  (pprint-logical-block (str obj)
    (format str "(~W ~W" (first obj) (second obj))
    (mapc #'(lambda (sub)
              (format str "~%~2i ~W" sub))
          (cddr obj))
    (format str ")")))


(set-pprint-dispatch '(satisfies toplevel-p)
                     #'toplevel-printer
                     0
                     *pddl-pprint-dispatch*)

(set-pprint-dispatch '(cons (member :requirements))
                     #'(lambda (str obj)
                         (pprint-logical-block (str obj)
                           (format str "(:REQUIREMENTS ~{~W~^ ~_~})"
                                   (rest obj))))
                     0
                     *pddl-pprint-dispatch*)

(set-pprint-dispatch '(cons (member :objects :constants :types))
                     #'(lambda (str obj)
                         (if *canonical*
                             (let ((sorted-objs
                                     (canonicalize-types (rest obj))))
                               (pprint-logical-block (str obj)
                                 (format str "(~S" (first obj))
                                 (pprint-indent :block 2 str)
                                 (loop for sublist in (min-canonical-sublists sorted-objs)
                                       do (assert (and (= (length sublist) 3)
                                                       (eq (second sublist) '-)))
                                       do (pprint-newline :mandatory str)
                                          (pprint-logical-block (str sublist)
                                            (pprint-indent :block 2 str)
                                            (let ((entities (reverse (cddr (reverse sublist))))
                                                  (type (first (reverse sublist))))
                                              (format str "~{~W~^ ~:_~}" entities)
                                              (format str " - ~W~:_" type))))
                                 (format str ")")))
                             ;; minimal form, instead of canonical form
                             (let ((sorted-objs
                                     (minimize-canonical-type-list
                                      (canonicalize-types (rest obj)))))
                               (pprint-logical-block (str obj)
                                 (format str "(~S" (first obj))
                                 (pprint-indent :block 2 str)
                                 (loop for sublist in (min-canonical-sublists sorted-objs)
                                       do (pprint-newline :mandatory str)
                                          (pprint-logical-block (str sublist)
                                            (pprint-indent :block 2 str)
                                            (let ((entities (reverse (cddr (reverse sublist))))
                                                  (type (first (reverse sublist))))
                                              (format str "~{~W~^ ~:_~}" entities)
                                              (format str " - ~W~:_" type))))
                                 (format str ")")))))
                     0 *pddl-pprint-dispatch*)

(defun quantified-p (x)
  (typep x `(cons (member ,(uiop:find-symbol* '#:forall *pddl-package*)
                          ,(uiop:find-symbol* '#:exists *pddl-package*)))))

(set-pprint-dispatch '(satisfies quantified-p)
                     #'(lambda (str obj)
                         (destructuring-bind (quantifier varlist expr) obj
                           (let ((sorted-vars
                                   (if *canonical*
                                       (canonicalize-types varlist)
                                       (minimize-canonical-type-list
                                        (canonicalize-types varlist)))))
                             (pprint-logical-block (str obj)
                               (format str "(~S " quantifier)
                               (pprint-logical-block (str varlist)
                                 (format str "(")
                                 (pprint-indent :block 1 str)
                                 (loop for (sublist . rest) on (min-canonical-sublists sorted-vars)
                                       do (pprint-logical-block (str sublist)
                                            (format str "~{~W~^ ~}" sublist)
                                            (when rest (format str " ")))
                                       do (pprint-newline :fill str))
                                 (format str ")"))
                               (pprint-indent :block 2 str)
                               (pprint-newline :mandatory str)
                               (pprint-logical-block (str expr)
                                 (write expr :stream str))
                               (format str ")")))))
                     0 *pddl-pprint-dispatch*)



(set-pprint-dispatch '(cons (member :init))
                      #'(lambda (str obj)
                          (pprint-logical-block (str obj)
                            (format str "(~S" (first obj))
                            (pprint-indent :block 2 str)
                            (loop for sublist in (rest obj)
                                  do (pprint-newline :mandatory str)
                                     (pprint-logical-block (str sublist)
                                       (write sublist :stream str)))
                            (format str ")")))
                      0 *pddl-pprint-dispatch*)

(defun bool-fun-p (x)
  (typep x `(cons (member ,(uiop:find-symbol* '#:and *pddl-package*)
                          ,(uiop:find-symbol* '#:or *pddl-package*)
                          ;; these 2 binary only dunno if that's different
                          ,(uiop:find-symbol* '#:imply *pddl-package*)
                          ,(uiop:find-symbol* '#:when *pddl-package*)))))

(set-pprint-dispatch '(satisfies bool-fun-p) 
                      #'(lambda (str obj)
                          (pprint-logical-block (str obj)
                            (format str "(~S" (first obj))
                            (pprint-indent :block 2 str)
                            (loop for sublist in (rest obj)
                                  do (pprint-newline :mandatory str)
                                  (pprint-logical-block (str sublist)
                                    (write sublist :stream str)))
                            (format str ")")))
                      0 *pddl-pprint-dispatch*)

;;; FIXME: need a pprint dispatcher for :action so that we get the actions
;;; formatted nicely with the keywords all starting new lines.  Also need to
;;; make sure that the typed lists of parameters get formatted properly.


;;;---------------------------------------------------------------------------
;;;  Miscellaneous helpers
;;;---------------------------------------------------------------------------

(defun canonicalize-types (typed-list)
  "Return a new typed list that comes from taking all of the \(irregular\)
elements of TYPED-LIST and smashing them flat to <TYPED> - <TYPE> entries."
  (loop with lst = typed-list
        while lst
        for pos = (position '- lst)
        if (null pos)
        append (loop for name in lst
                     append `(,name - ,(pddl-symbol '#:object)))
        ;; we are done with the loop
        and do (setf lst nil)
        else
        append (loop for name in (subseq lst 0 pos)
                     with type = (nth (1+ pos) lst)
                     append `(,name - ,type))
        and do (setf lst (subseq lst (+ pos 2)))))

(defun minimize-canonical-type-list (typed-list)
  "Take a canonicalized type list and minimize it."
  (let ((bucketed
         (make-hash-table :test 'eq))
        keys)
    (loop for (name sep type . nil) on typed-list by #'cdddr
          with bucket
          unless (eq sep '-)
          do (error "Not canonical typed-list")
          do (setf bucket (gethash type bucketed))
          unless (member name bucket :test 'eq)
          do (setf (gethash type bucketed) (cons name bucket)))
    (maphash #'(lambda (k v)
                 (declare (ignore v))
                 (push k keys)) bucketed)
    (setf keys (sort keys 'string-lessp))
    (loop for k in keys
          as names = (gethash k bucketed)
          as sorted = (sort names 'string-lessp)
          appending `(,@sorted - ,k))))


(defun min-canonical-sublists (mctl)
  "Take a minimized, canonical, type list and break it up into
entries for individual printing."
  (loop with lst = mctl
        while lst
        for pos = (position '- lst)
        unless pos
        do (error "Ill-formed typelist.")
        collect (subseq lst 0 (+ pos 2))
        do (setf lst (subseq lst (+ pos 2)))))
