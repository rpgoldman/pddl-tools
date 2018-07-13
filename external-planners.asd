(defpackage :sift-external-planners-asd
  (:use :common-lisp :asdf))

(in-package :sift-external-planners-asd)

(defsystem :external-planners
    :name "SIFT-EXTERNAL-PLANNERS"
    :version "1.0"
    :license "BSD 3-clause (see license.txt)"
    :weakly-depends-on (fd ff)
    :pathname "planners/"
    :components ((:file "package")
                 ;; Will hold the variables speciying relative paths to the binaries in
                 ;; the distribution directories.
                 (:file "generics" :depends-on ("package"))
                 ))
