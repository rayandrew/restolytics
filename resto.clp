(deftemplate restaurant 
  (slot name)
  (slot isSmoker)
  (slot minBudget)
  (slot maxBudget)
  (slot dresscode1)
  (slot dresscode2)
  (slot hasWifi)
  (slot latitude)
  (slot longitude))

(deftemplate count
  (slot name)
  (slot point)
  (slot distance))

(deftemplate counter
  (slot name)
  (slot value))

(deftemplate priority
  (slot name)
  (slot recommendation)
  (slot distance))

;Startup
(defrule startup
  =>
  (assert (restaurant (name A) (isSmoker "yes") (minBudget 1000) (maxBudget 2000) (dresscode1 "casual") (dresscode2 "none") (hasWifi "yes") (latitude -6.8922186) (longitude 107.5886173)))
  (assert (restaurant (name B) (isSmoker "no") (minBudget 1200) (maxBudget 2500) (dresscode1 "informal") (dresscode2 "none") (hasWifi "yes") (latitude -6.224085) (longitude 106.7859815)))
  (assert (restaurant (name C) (isSmoker "yes") (minBudget 2000) (maxBudget 4000) (dresscode1 "formal") (dresscode2 "none") (hasWifi "no") (latitude -6.2145285) (longitude 106.8642591)))
  (assert (restaurant (name D) (isSmoker "no") (minBudget 500) (maxBudget 1400) (dresscode1 "formal") (dresscode2 "none") (hasWifi "no") (latitude -6.9005363) (longitude 107.6222191))) 
  (assert (restaurant (name E) (isSmoker "yes") (minBudget 1000) (maxBudget 2000) (dresscode1 "casual") (dresscode2  "informal") (hasWifi "yes") (latitude -6.2055617) (longitude 106.8001597)))
  (assert (restaurant (name F) (isSmoker "no") (minBudget 2500) (maxBudget 5000) (dresscode1 "informal") (dresscode2 "none") (hasWifi "yes") (latitude -6.9045679) (longitude 107.6399745)))
  (assert (restaurant (name G) (isSmoker "yes") (minBudget 1300) (maxBudget 3000) (dresscode1 "casual") (dresscode2 "none") (hasWifi "yes") (latitude -6.1881082) (longitude 106.7844409)))
  (assert (restaurant (name H) (isSmoker "no") (minBudget 400) (maxBudget 1000) (dresscode1 "inrofmal") (dresscode2 "none") (hasWifi "no") (latitude -6.9525133) (longitude 107.6052906)))
  (assert (restaurant (name I) (isSmoker "no") (minBudget 750) (maxBudget 2200) (dresscode1 "informal") (dresscode2 "casual") (hasWifi "yes") (latitude -6.9586985) (longitude 107.7092281)))
  (assert (restaurant (name J) (isSmoker "no") (minBudget 1500) (maxBudget 2000) (dresscode1 "casual") (dresscode2 "none") (hasWifi "yes") (latitude -6.2769732) (longitude 106.775133)))
  (assert (get-name))
  (assert (counter (name "sort") (value 0)))
  (assert (counter (name "print") (value 0))))

; Menerima Input Nama
(defrule get-name
  ?f <- (get-name)
  =>
  (printout t "What is your name? ")
  (bind ?input (readline))
  (retract ?f)
  (assert (name ?input))
  (assert (get-smoke)))

;Menerima Input Smoke
(defrule get-smoke
  ?f <- (get-smoke)
  =>
  (printout t "Do you smoke? (yes, no) ")
  (bind ?input (readline))
  (if (and (neq ?input "yes") (neq ?input "no"))
    then
    (retract ?f)
    (assert (get-smoke))
    else
    (retract ?f)
    (assert (smoke ?input))
    (assert (get-min-budget))))

;Menerima minimum budget
(defrule get-min-budget
  ?f <- (get-min-budget)
  =>
  (printout t "What is your minimum budget? [0-9999] ")
  (bind ?input (readline))
  (if (or (< (nth$ 1 (explode$ ?input)) 0) (> (nth$ 1 (explode$ ?input)) 9999))
    then
    (retract ?f)
    (assert (get-min-budget))
    else
    (retract ?f)
    (assert (min-budget (nth$ 1 (explode$ ?input))))
    (assert (get-max-budget))))

;Menerima maximum budget
(defrule get-max-budget
  ?f <- (get-max-budget)
    ?min <- (min-budget ?x)
  =>
  (printout t "What is your maximum budget? [0-9999] ")
  (bind ?input (readline))
  (if (or (or (< (nth$ 1 (explode$ ?input)) 0) (> (nth$ 1 (explode$ ?input)) 9999))
             (< (nth$ 1 (explode$ ?input)) ?x))
    then
    (retract ?f)
    (assert (get-max-budget))
    else
    (retract ?f)
    (assert (max-budget (nth$ 1 (explode$ ?input))))
        (assert (get-clothes))))

;Menerima input clothes
(defrule get-clothes
  ?f <- (get-clothes)
  =>
  (printout t "What clothes are you wearing? (casual, informal, formal) ")
  (bind ?input (readline))
  (if (and (neq ?input "casual") (neq ?input "informal") (neq ?input "formal"))
    then
    (retract ?f)
    (assert (get-clothes))
    else
    (retract ?f)
    (assert (clothes ?input))
    (assert (get-wifi))))

;Menerima preferensi wifi
(defrule get-wifi
  ?f <- (get-wifi)
  =>
  (printout t "Do you want a restaurant with Wi-Fi? (yes, no) ")
  (bind ?input (readline))
  (if (and (neq ?input "yes") (neq ?input "no"))
    then
    (retract ?f)
    (assert (get-wifi))
    else
    (retract ?f)
    (assert (wifi ?input)))
    (assert (get-lat)))

;Menerima lat. coordinate
(defrule get-lat
  ?f <- (get-lat)
  =>
  (printout t "What are your lat. coordinate? ")
  (bind ?input (readline))
  (if (or (< (nth$ 1 (explode$ ?input)) -999) (> (nth$ 1 (explode$ ?input)) 999))
    then
    (retract ?f)
    (assert (get-lat))
    else
    (retract ?f)
    (assert (lat (nth$ 1 (explode$ ?input)))))
    (assert (get-long)))

;Menerima long. coordinate
(defrule get-long
  ?f <- (get-long)
  =>
  (printout t "What are your long. coordinate? ")
  (bind ?input (readline))
  (if (or (< (nth$ 1 (explode$ ?input)) -999) (> (nth$ 1 (explode$ ?input)) 999))
    then
    (retract ?f)
    (assert (get-long))
    else
    (retract ?f)
    (assert (long (nth$ 1 (explode$ ?input))))))

;point
(defrule calculate-point
  (restaurant (name ?nameRes) (isSmoker ?bool) (minBudget ?min) (maxBudget ?max) (dresscode1 ?use1) (dresscode2 ?use2) (hasWifi ?has) (latitude ?x) (longitude ?y))
  (smoke ?boolSmoke)
  (min-budget ?minB)
  (max-budget ?maxB)
  (clothes ?clot)
  (wifi ?wf)
  (lat ?lat)
  (long ?long)
  =>
  (bind ?total 0)
  (if (eq ?bool ?boolSmoke)
    then
    (bind ?total (+ ?total 1)))
  (if (>= ?min ?minB)
    then
    (bind ?total (+ ?total 1)))
  (if (<= ?max ?maxB)
    then
    (bind ?total (+ ?total 1)))
  (if (or (eq ?use1 ?clot) (eq ?use1 ?clot))
    then
    (bind ?total (+ ?total 1)))
  (if (eq ?wf ?has)
    then
    (bind ?total (+ ?total 1)))
  (bind ?distance (sqrt (+ (** (- ?lat ?x) 2) (** (- ?long ?y) 2))))
  (assert (count (name ?nameRes) (point ?total) (distance ?distance))))

;sort
(defrule unsorted
  (count (name ?name))
  =>
  (assert (unsorted ?name))
)

(defrule take-three
  (declare (salience -10))
  ?f-1 <- (unsorted ?name)
  ?f-2 <- (counter (name "sort") (value ?value))
  (test (< ?value 3))
  (count (name ?name) (point ?point) (distance ?distance))
  (forall (and (unsorted ?nameRes) (count (name ?nameRes) (point ?pointRes) (distance ?distanceRes)))
    (test (<= ?pointRes ?point)))
  (forall (and (unsorted ?nameRes) (count (name ?nameRes) (point ?point) (distance ?distanceRes)))
    (test (<= ?distance ?distanceRes)))
  =>
  (retract ?f-1)
  (modify ?f-2 (value (+ ?value 1)))
  (if (= ?point 5)
    then
    (assert (priority (name ?name) (recommendation "Very recommendable") (distance ?distance)))
    else
    (if (>= ?point 3)
      then
      (assert (priority (name ?name) (recommendation "Recommendable") (distance ?distance)))
      else
      (assert (priority (name ?name) (recommendation "Not recommendable") (distance ?distance))))))

;print
(defrule print
  ?f-1 <- (priority (name ?name) (recommendation ?recommendation) (distance ?distance))
  ?f-2 <- (counter (name "print") (value ?value))
  =>
  (retract ?f-1)
  (modify ?f-2 (value (+ ?value 1)))
  (printout t (+ ?value 1) ". Restaurant " ?name " : " ?recommendation " (" ?distance ")" crlf))

(defrule delete-fact-unsorted
  (declare (salience 10))
  ?f-1 <- (unsorted ?name)
  (counter (name "sort") (value 3))
  (count (name ?name) (point ?point) (distance ?distance))
  =>
  (retract ?f-1))

(defrule delete-fact-counter
  ?f-1 <- (counter (name "sort") (value 3))
  ?f-2 <- (counter (name "print") (value 3))
  =>
  (retract ?f-1)
  (retract ?f-2))