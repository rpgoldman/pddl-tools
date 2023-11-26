(in-package :common-lisp-user)

(defpackage :hddl-utils-tests
  (:use common-lisp hddl-utils fiveam)
  (:import-from :alexandria #:set-equal)
  (:import-from :hddl-io #:partition-method-line
                #:read-hddl-file
                #:complex-task-sexp-p))

(in-package :hddl)

(cl:defparameter hddl-utils-tests::*complex-task-def*
  '(:task deliver :parameters (?p - package ?l - location)))

(cl:defparameter hddl-utils-tests::*complex-task-def2*
  '(:task deliver :parameters (?p - package ?l - location)))

(cl:defparameter hddl-utils-tests::*method*
  '(:task get-to :parameters (?l - location)))

(cl:defparameter hddl-utils-tests::*parsed-plan*
  '(:HDDL-PLAN :ACTIONS
 ((0 DRIVE TRUCK-0 CITY-LOC-2
   CITY-LOC-1)
  (1 PICK-UP TRUCK-0 CITY-LOC-1
   PACKAGE-0 CAPACITY-0 CAPACITY-1)
  (2 DRIVE TRUCK-0 CITY-LOC-1
   CITY-LOC-0)
  (3 DROP TRUCK-0 CITY-LOC-0
   PACKAGE-0 CAPACITY-0 CAPACITY-1)
  (4 DRIVE TRUCK-0 CITY-LOC-0
   CITY-LOC-1)
  (5 PICK-UP TRUCK-0 CITY-LOC-1
   PACKAGE-1 CAPACITY-0 CAPACITY-1)
  (6 DRIVE TRUCK-0 CITY-LOC-1
   CITY-LOC-2)
  (7 DROP TRUCK-0 CITY-LOC-2
   PACKAGE-1 CAPACITY-0 CAPACITY-1))
 :ROOTS (15 14) :DECOMPOSITIONS
 ((8
   (LOAD TRUCK-0 CITY-LOC-1
    PACKAGE-0)
   M-LOAD 1)
  (9
   (UNLOAD TRUCK-0 CITY-LOC-0
    PACKAGE-0)
   M-UNLOAD 3)
  (10 (GET-TO TRUCK-0 CITY-LOC-1)
   M-DRIVE-TO 0)
  (11
   (UNLOAD TRUCK-0 CITY-LOC-2
    PACKAGE-1)
   M-UNLOAD 7)
  (12 (GET-TO TRUCK-0 CITY-LOC-0)
   M-DRIVE-TO 2)
  (13
   (LOAD TRUCK-0 CITY-LOC-1
    PACKAGE-1)
   M-LOAD 5)
  (14 (DELIVER PACKAGE-0 CITY-LOC-0)
   M-DELIVER 10 8 12 9)
  (15 (DELIVER PACKAGE-1 CITY-LOC-2)
   M-DELIVER 16 13 17 11)
  (16 (GET-TO TRUCK-0 CITY-LOC-1)
   M-DRIVE-TO 4)
  (17 (GET-TO TRUCK-0 CITY-LOC-2)
   M-DRIVE-TO 6))))

(cl:defparameter hddl-utils-tests::*expected-task*
  '(drive truck-0 city-loc-2 city-loc-1))

(cl:defparameter hddl-utils-tests::*full-method*
  '(:method achieve-communicated-image-data
    :parameters (?obj - objective ?mode - mode ?rover - rover ?l - lander ?photo-loc ?lander-loc - location
                 ?camera - camera)
    :task
    (communicate-image-data ?obj ?mode ?rover) :precondition
    (and
     (on_board ?camera ?rover)
     (supports ?camera ?mode)
     (at_lander ?l ?lander-loc))
    :ordered-subtasks
    (and
     (calibrate-camera ?rover ?camera)
     (get-line-of-sight ?rover ?obj ?photo-loc)
     (take_image ?rover ?photo-loc ?obj ?camera ?mode)
     (communicate-image ?photo-loc ?lander-loc ?rover ?obj ?mode)))
  )

(cl:defparameter hddl-utils-tests::*method-subtasks*
  '(and
    (calibrate-camera ?rover ?camera)
    (get-line-of-sight ?rover ?obj ?photo-loc)
    (take_image ?rover ?photo-loc ?obj ?camera ?mode)
    (communicate-image ?photo-loc ?lander-loc ?rover ?obj ?mode)))

(cl:defparameter hddl-utils-tests::*method-no-subtasks*
  '(:method achieve-communicated-image-data
    :parameters (?obj - objective ?mode - mode ?rover - rover ?l - lander ?photo-loc ?lander-loc - location
                 ?camera - camera)
    :task
    (communicate-image-data ?obj ?mode ?rover) :precondition
    (and
     (on_board ?camera ?rover)
     (supports ?camera ?mode)
     (at_lander ?l ?lander-loc))
    :ordered-subtasks nil))

(cl:defparameter hddl-utils-tests::*method-different-subtasks*
  '(:method achieve-communicated-image-data
    :parameters (?obj - objective ?mode - mode ?rover - rover ?l - lander ?photo-loc ?lander-loc - location
                 ?camera - camera)
    :task
    (communicate-image-data ?obj ?mode ?rover) :precondition
    (and
     (on_board ?camera ?rover)
     (supports ?camera ?mode)
     (at_lander ?l ?lander-loc))
    :ordered-subtasks (communicate-image ?photo-loc ?lander-loc ?rover ?obj ?mode)))
