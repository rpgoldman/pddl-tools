#|
 This file is a part of pddl-tools
 (c) 2023 Robert P. Goldman and SIFT, LLC
 Author: Robert P. Goldman <rpgoldman@sift.net>
|#


(in-package :common-lisp-user)

(defpackage test
  (:use :common-lisp))

(in-package :test)

(let ((home-dir (user-homedir-pathname)))
  (let ((path (uiop:ensure-directory-pathname home-dir)))
    (load (merge-pathnames "quicklisp/setup.lisp" path))))

(push (namestring (uiop:pathname-directory-pathname *load-truename*)) ql:*local-project-directories*)

(ql:quickload "fiveam-asdf")            ; without this, the ASDF defsystems don't load properly.

#+nil
(handler-case
    (assert (uiop:pathname-equal (uiop:pathname-directory-pathname (asdf:component-pathname (asdf:find-system "pddl-utils")))
                                 (uiop:pathname-directory-pathname *load-truename*)))
  (error () (uiop:die 2 "Not loading PDDL-tools from the expected location: ~A instead of expected ~a."
                      (asdf:component-pathname (asdf:find-system "pddl-utils"))
                      )))

;;; check for clean build
(defmacro build-system (name)
  `(handler-bind
       ((error #'(lambda (c) (uiop:die 3 "Failed to build ~a cleanly with error:~%~T~a" ,name c))))
     (let ((asdf:*compile-file-failure-behaviour* :error)
           (asdf:*compile-file-warnings-behaviour* :error))
       (ql:quickload ,name :silent nil :verbose t))))

;;; these are done outside the bounds of BUILD-SYSTEM below, because
;;; CL-PPCRE doesn't build clean (at least on SBCL) and CL-JSON does not
;;; build clean on Allegro
(ql:quickload "cl-ppcre")
(ql:quickload "cl-json")


(build-system "pddl-utils")
(build-system "hddl-utils")
(build-system "hddl-to-json")

(ql:quickload "pddl-utils/tests")
(ql:quickload "hddl-utils/tests")
(ql:quickload "hddl-to-json/tests")

(defmacro test-system (name)
  `(handler-case
       (asdf:test-system ,name)
     (fiveam-asdf:fiveam-test-fail ()
       (uiop:quit 1))))

(test-system "pddl-utils")
(test-system "hddl-utils")
(test-system "hddl-to-json")

(uiop:quit 0)
