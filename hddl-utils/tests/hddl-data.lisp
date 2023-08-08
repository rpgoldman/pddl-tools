(in-package :hddl)

(cl:defparameter hddl-utils-tests::*complex-task-def*
  '(:task deliver :parameters (?p - package ?l - location)))

(cl:defparameter hddl-utils-tests::*complex-task-def2*
  '(:task deliver :parameters (?p - package ?l - location)))

(cl:defparameter hddl-utils-tests::*method*
  '(:task get-to :parameters (?l - location)))

#|
(cl:defparameter pddl-utils-tests::*pddl-keywords*
  '(:adl :strips 
    :typing 
    :negative-preconditions
    :disjunctive-preconditions
    :equality
    :existential-preconditions
    :universal-preconditions
    :quantified-preconditions 
    :existential-preconditions
    :universal-preconditions
    :conditional-effects
    :fluents
    :durative-actions
    :duration-inequalities
    :continuous-effects))


(cl:defparameter pddl-utils-tests::*p01-state*
  '(

    (at-segment airplane_CFBEG seg_rw_0_400)

    (blocked seg_rw_0_400 airplane_CFBEG)
    (blocked seg_rwe_0_50 airplane_CFBEG)

    (can-move seg_pp_0_60 seg_ppdoor_0_40 north)
    (can-move seg_ppdoor_0_40 seg_tww1_0_200 north)
    (can-move seg_tww1_0_200 seg_twe1_0_200 north)
    (can-move seg_twe1_0_200 seg_twe2_0_50 south)
    (can-move seg_twe2_0_50 seg_twe3_0_50 south)
    (can-move seg_twe3_0_50 seg_twe4_0_50 south)
    (can-move seg_twe4_0_50 seg_rwe_0_50 south)
    (can-move seg_rwe_0_50 seg_rw_0_400 south)
    (can-move seg_rw_0_400 seg_rww_0_50 south)
    (can-move seg_rww_0_50 seg_tww4_0_50 south)
    (can-move seg_tww4_0_50 seg_tww3_0_50 north)
    (can-move seg_tww3_0_50 seg_tww2_0_50 north)
    (can-move seg_tww2_0_50 seg_tww1_0_200 north)
    (can-move seg_tww1_0_200 seg_ppdoor_0_40 north)
    (can-move seg_ppdoor_0_40 seg_pp_0_60 south)


    (facing airplane_CFBEG south)

    (has-type airplane_CFBEG medium)

    (is-blocked seg_ppdoor_0_40 medium seg_pp_0_60 south)
    (is-blocked seg_pp_0_60 medium seg_ppdoor_0_40 north)
    (is-blocked seg_tww1_0_200 medium seg_ppdoor_0_40 south)
    (is-blocked seg_twe1_0_200 medium seg_ppdoor_0_40 south)
    (is-blocked seg_tww2_0_50 medium seg_tww1_0_200 north)
    (is-blocked seg_ppdoor_0_40 medium seg_tww1_0_200 south)
    (is-blocked seg_twe1_0_200 medium seg_tww1_0_200 south)
    (is-blocked seg_twe2_0_50 medium seg_twe1_0_200 north)
    (is-blocked seg_ppdoor_0_40 medium seg_twe1_0_200 south)
    (is-blocked seg_tww1_0_200 medium seg_twe1_0_200 south)
    (is-blocked seg_tww3_0_50 medium seg_tww2_0_50 north)
    (is-blocked seg_tww1_0_200 medium seg_tww2_0_50 south)
    (is-blocked seg_tww4_0_50 medium seg_tww3_0_50 north)
    (is-blocked seg_tww2_0_50 medium seg_tww3_0_50 south)
    (is-blocked seg_rww_0_50 medium seg_tww4_0_50 north)
    (is-blocked seg_rwtw1_0_10 medium seg_tww4_0_50 north)
    (is-blocked seg_tww3_0_50 medium seg_tww4_0_50 south)
    (is-blocked seg_rwte1_0_10 medium seg_twe4_0_50 north)
    (is-blocked seg_rwe_0_50 medium seg_twe4_0_50 north)
    (is-blocked seg_twe3_0_50 medium seg_twe4_0_50 south)
    (is-blocked seg_twe4_0_50 medium seg_twe3_0_50 north)
    (is-blocked seg_twe2_0_50 medium seg_twe3_0_50 south)
    (is-blocked seg_twe3_0_50 medium seg_twe2_0_50 north)
    (is-blocked seg_twe1_0_200 medium seg_twe2_0_50 south)
    (is-blocked seg_tww4_0_50 medium seg_rww_0_50 north)
    (is-blocked seg_rww_0_50 medium seg_rw_0_400 north)
    (is-blocked seg_rww_0_50 medium seg_rwe_0_50 north)
    (is-blocked seg_rw_0_400 medium seg_rwe_0_50 north)
    (is-blocked seg_twe4_0_50 medium seg_rwe_0_50 south)
    (is-blocked seg_rwe_0_50 medium seg_rw_0_400 south)
    (is-blocked seg_rwe_0_50 medium seg_rww_0_50 south)
    (is-blocked seg_rw_0_400 medium seg_rww_0_50 south)

    (is-moving airplane_CFBEG)


    (is-start-runway seg_rww_0_50 north)
    (is-start-runway seg_rwe_0_50 south)



    (move-dir seg_pp_0_60 seg_ppdoor_0_40 north)
    (move-dir seg_ppdoor_0_40 seg_tww1_0_200 south)
    (move-dir seg_tww1_0_200 seg_twe1_0_200 south)
    (move-dir seg_twe1_0_200 seg_twe2_0_50 south)
    (move-dir seg_twe2_0_50 seg_twe3_0_50 south)
    (move-dir seg_twe3_0_50 seg_twe4_0_50 south)
    (move-dir seg_twe4_0_50 seg_rwe_0_50 south)
    (move-dir seg_rwe_0_50 seg_rw_0_400 south)
    (move-dir seg_rw_0_400 seg_rww_0_50 south)
    (move-dir seg_rww_0_50 seg_tww4_0_50 north)
    (move-dir seg_tww4_0_50 seg_tww3_0_50 north)
    (move-dir seg_tww3_0_50 seg_tww2_0_50 north)
    (move-dir seg_tww2_0_50 seg_tww1_0_200 north)
    (move-dir seg_tww1_0_200 seg_ppdoor_0_40 south)
    (move-dir seg_ppdoor_0_40 seg_pp_0_60 south)

    (occupied seg_rw_0_400)
    ))

(cl:defparameter pddl-utils-tests::*substituted-p01-state*
  '(

    (at-segment ga1 seg_rw_0_400)

    (blocked seg_rw_0_400 ga1)
    (blocked seg_rwe_0_50 ga1)

    (can-move seg_pp_0_60 seg_ppdoor_0_40 north)
    (can-move seg_ppdoor_0_40 seg_tww1_0_200 north)
    (can-move seg_tww1_0_200 seg_twe1_0_200 north)
    (can-move seg_twe1_0_200 seg_twe2_0_50 south)
    (can-move seg_twe2_0_50 seg_twe3_0_50 south)
    (can-move seg_twe3_0_50 seg_twe4_0_50 south)
    (can-move seg_twe4_0_50 seg_rwe_0_50 south)
    (can-move seg_rwe_0_50 seg_rw_0_400 south)
    (can-move seg_rw_0_400 seg_rww_0_50 south)
    (can-move seg_rww_0_50 seg_tww4_0_50 south)
    (can-move seg_tww4_0_50 seg_tww3_0_50 north)
    (can-move seg_tww3_0_50 seg_tww2_0_50 north)
    (can-move seg_tww2_0_50 seg_tww1_0_200 north)
    (can-move seg_tww1_0_200 seg_ppdoor_0_40 north)
    (can-move seg_ppdoor_0_40 seg_pp_0_60 south)


    (facing ga1 south)

    (has-type ga1 medium)

    (is-blocked seg_ppdoor_0_40 medium seg_pp_0_60 south)
    (is-blocked seg_pp_0_60 medium seg_ppdoor_0_40 north)
    (is-blocked seg_tww1_0_200 medium seg_ppdoor_0_40 south)
    (is-blocked seg_twe1_0_200 medium seg_ppdoor_0_40 south)
    (is-blocked seg_tww2_0_50 medium seg_tww1_0_200 north)
    (is-blocked seg_ppdoor_0_40 medium seg_tww1_0_200 south)
    (is-blocked seg_twe1_0_200 medium seg_tww1_0_200 south)
    (is-blocked seg_twe2_0_50 medium seg_twe1_0_200 north)
    (is-blocked seg_ppdoor_0_40 medium seg_twe1_0_200 south)
    (is-blocked seg_tww1_0_200 medium seg_twe1_0_200 south)
    (is-blocked seg_tww3_0_50 medium seg_tww2_0_50 north)
    (is-blocked seg_tww1_0_200 medium seg_tww2_0_50 south)
    (is-blocked seg_tww4_0_50 medium seg_tww3_0_50 north)
    (is-blocked seg_tww2_0_50 medium seg_tww3_0_50 south)
    (is-blocked seg_rww_0_50 medium seg_tww4_0_50 north)
    (is-blocked seg_rwtw1_0_10 medium seg_tww4_0_50 north)
    (is-blocked seg_tww3_0_50 medium seg_tww4_0_50 south)
    (is-blocked seg_rwte1_0_10 medium seg_twe4_0_50 north)
    (is-blocked seg_rwe_0_50 medium seg_twe4_0_50 north)
    (is-blocked seg_twe3_0_50 medium seg_twe4_0_50 south)
    (is-blocked seg_twe4_0_50 medium seg_twe3_0_50 north)
    (is-blocked seg_twe2_0_50 medium seg_twe3_0_50 south)
    (is-blocked seg_twe3_0_50 medium seg_twe2_0_50 north)
    (is-blocked seg_twe1_0_200 medium seg_twe2_0_50 south)
    (is-blocked seg_tww4_0_50 medium seg_rww_0_50 north)
    (is-blocked seg_rww_0_50 medium seg_rw_0_400 north)
    (is-blocked seg_rww_0_50 medium seg_rwe_0_50 north)
    (is-blocked seg_rw_0_400 medium seg_rwe_0_50 north)
    (is-blocked seg_twe4_0_50 medium seg_rwe_0_50 south)
    (is-blocked seg_rwe_0_50 medium seg_rw_0_400 south)
    (is-blocked seg_rwe_0_50 medium seg_rww_0_50 south)
    (is-blocked seg_rw_0_400 medium seg_rww_0_50 south)

    (is-moving ga1)


    (is-start-runway seg_rww_0_50 north)
    (is-start-runway seg_rwe_0_50 south)



    (move-dir seg_pp_0_60 seg_ppdoor_0_40 north)
    (move-dir seg_ppdoor_0_40 seg_tww1_0_200 south)
    (move-dir seg_tww1_0_200 seg_twe1_0_200 south)
    (move-dir seg_twe1_0_200 seg_twe2_0_50 south)
    (move-dir seg_twe2_0_50 seg_twe3_0_50 south)
    (move-dir seg_twe3_0_50 seg_twe4_0_50 south)
    (move-dir seg_twe4_0_50 seg_rwe_0_50 south)
    (move-dir seg_rwe_0_50 seg_rw_0_400 south)
    (move-dir seg_rw_0_400 seg_rww_0_50 south)
    (move-dir seg_rww_0_50 seg_tww4_0_50 north)
    (move-dir seg_tww4_0_50 seg_tww3_0_50 north)
    (move-dir seg_tww3_0_50 seg_tww2_0_50 north)
    (move-dir seg_tww2_0_50 seg_tww1_0_200 north)
    (move-dir seg_tww1_0_200 seg_ppdoor_0_40 south)
    (move-dir seg_ppdoor_0_40 seg_pp_0_60 south)

    (occupied seg_rw_0_400)
    ))

(cl:defparameter pddl-utils-tests::*actionless-domain*
  '(define (domain airport)
    (:requirements :adl)

    (:types segment direction airplanetype - object
     vehicle - object ; new type
     airplane - vehicle ; redefined type
                                        ;truck ; new type
                                        ; - vehicle
     )

    (:predicates
     (can-move ?s1 ?s2 - segment ?d - direction)
     (can-pushback ?s1 ?s2 - segment ?d - direction)
     (move-dir ?s1 ?s2 - segment ?d - direction)
     (move-back-dir ?s1 ?s2 - segment ?d - direction)
     (is-blocked ?s1  - segment ?t - airplanetype ?s2 - segment ?d - direction)
     (has-type ?a - airplane ?t - airplanetype)
     (at-segment ?v - vehicle ?s - segment)
     (facing ?a - airplane ?d - direction)
     (occupied ?s - segment)
     (blocked ?s - segment ?a - vehicle)
     (is-start-runway ?s - segment ?d - direction)
     (airborne ?a - airplane ?s - segment)
     (is-moving ?a - airplane)
     (is-pushing ?a - airplane)
     (is-parked ?a - airplane ?s - segment)
     )
    ))

(cl:defparameter pddl-utils-tests::*airport-action-list*
  '((:action move
 :parameters
     (?a - airplane ?t - airplanetype ?d1 - direction ?s1 ?s2  - segment ?d2 - direction)
 :precondition
     (and
                (has-type ?a ?t)
                (is-moving ?a)
                (not (= ?s1 ?s2))
                (facing ?a ?d1)
                (can-move ?s1 ?s2 ?d1)
                (move-dir ?s1 ?s2 ?d2)
                (at-segment ?a ?s1)
                (not    (exists (?a1 - vehicle) (and    (not (= ?a1 ?a))
                                                                (blocked ?s2 ?a1))))
                (forall (?s - segment)  (imply  (and    (is-blocked ?s ?t ?s2 ?d2)
                                                        (not (= ?s ?s1)))
                                        (not (occupied ?s))
                                ))
                )
 :effect
     (and
                (occupied ?s2)
                (blocked ?s2 ?a)
                (not (occupied ?s1))
                (when   (not (is-blocked ?s1 ?t ?s2 ?d2))
                        (not (blocked ?s1 ?a)))
                (when   (not (= ?d1 ?d2))
                        (not (facing ?a ?d1)))
                (not (at-segment ?a ?s1))
                (forall (?s - segment)  (when   (is-blocked ?s ?t ?s2 ?d2)
                                        (blocked ?s ?a)
                                ))
                (forall (?s - segment)  (when   (and    (is-blocked ?s ?t ?s1 ?d1)
                                                (not (= ?s ?s2))
                                                (not (is-blocked ?s ?t ?s2 ?d2))
                                        )
                                        (not (blocked ?s ?a))
                                ))
                (at-segment ?a ?s2)
                (when   (not (= ?d1 ?d2))
                        (facing ?a ?d2))
                )
)
(:action pushback
 :parameters
(?a - airplane ?t - airplanetype ?d1 - direction ?s1 ?s2  - segment ?d2 - direction)
 :precondition
(and
(has-type ?a ?t)
(is-pushing ?a)
(not (= ?s1 ?s2))
(facing ?a ?d1)
(can-pushback ?s1 ?s2 ?d1)
(move-back-dir ?s1 ?s2 ?d2)
(at-segment ?a ?s1)
(not    (exists (?a1 - vehicle) (and    (not (= ?a1 ?a))
                                                (blocked ?s2 ?a1))))
(forall (?s - segment)  (imply  (and    (is-blocked ?s ?t ?s2 ?d2)
                                        (not (= ?s ?s1)))
                        (not (occupied ?s))
                ))
)
 :effect
(and
(occupied ?s2)
(blocked ?s2 ?a)
(forall (?s - segment)  (when   (is-blocked ?s ?t ?s2 ?d2)
                        (blocked ?s ?a)
                ))
(forall (?s - segment)  (when   (and    (is-blocked ?s ?t ?s1 ?d1)
                                (not (= ?s ?s2))
                                (not (is-blocked ?s ?t ?s2 ?d2))
                        )
                        (not (blocked ?s ?a))
                ))
(at-segment ?a ?s2)
(when   (not (= ?d1 ?d2))
        (facing ?a ?d2))
(not (occupied ?s1))
(when   (not (is-blocked ?s1 ?t ?s2 ?d2))
        (not (blocked ?s1 ?a)))
(when   (not (= ?d1 ?d2))
        (not (facing ?a ?d1)))
(not (at-segment ?a ?s1))
)
)
(:action takeoff
 :parameters   (?a - airplane ?s - segment ?d - direction)
 :precondition (and
                (at-segment ?a ?s)
                (facing ?a ?d)
                (is-start-runway ?s ?d)
     )
 :effect (and (not (blocked ?s ?a))
              (not (occupied ?s))
              (not (at-segment ?a ?s))
              (airborne ?a ?s)
              (forall (?s1 - segment)  (when (blocked ?s1 ?a)
                                             (not (blocked ?s1 ?a))
                                       )
              )
         )
)
(:action park
 :parameters (?a - airplane ?t - airplanetype ?s - segment ?d - direction)
 :precondition (and (at-segment ?a ?s)
                    (facing ?a ?d)
                    (is-moving ?a)
               )
 :effect (and (is-parked ?a ?s)
              (not (is-moving ?a))
                (forall (?ss - segment) (when   (and    (is-blocked ?ss ?t ?s ?d)
                                                (not (= ?s ?ss))
                                                )
                                        (not (blocked ?ss ?a))
                                        )
              )
         )
)
(:action startup
 :parameters (?a - airplane)
 :precondition (is-pushing ?a)
 :effect (and (not (is-pushing ?a))
              (is-moving ?a) )
)
))
|#
