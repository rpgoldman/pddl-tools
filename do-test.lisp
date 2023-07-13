#|
 This file is a part of pddl-tools
 (c) 2023 Robert P. Goldman and SIFT, LLC
 Author: Robert P. Goldman <rpgoldman@sift.net>
|#


(in-package :common-lisp-user)

(defpackage test
  (:use :common-lisp))

(in-package :test)

(push (namestring (uiop:pathname-directory-pathname *load-truename*)) ql:*local-project-directories*)

(handler-case
 (assert (uiop:pathname-equal (asdf:component-pathname (asdf:find-system "pddl-utils"))
                              (uiop:pathname-directory-pathname *load-truename*)))
  (error () (uiop:die 2 "Not loading PDDL-tools from the expected location: ~A."
                       (asdf:component-pathname (asdf:find-system "pddl-utils")))))

;;; check for clean build
(defmacro build-system (name)
 `(handler-bind
     ((error #'(lambda (c) (uiop:die 3 "Failed to build ~a cleanly with error:~%~T~a" ,name c))))
   (let ((asdf:*compile-file-failure-behaviour* :error)
         (asdf:*compile-file-warnings-behaviour* :error))
     (ql:quickload ,name :silent nil :verbose t))))

(build-system "pddl-utils")
;; (build-system "hddl-utils")

(defmacro test-system (name)
  `(handler-case
    (asdf:test-system ,name)
  (asdf:fiveam-asdf-failure ()
    (uiop:quit 1))))

(test-system "pddl-utils")
;; (test-system "hddl-utils")

(uiop:quit 0)
