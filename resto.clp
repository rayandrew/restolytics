(deftemplate restaurant 
	(slot name)
	(slot isSmoker)
	(slot minBudget)
	(slot maxBudget)
	(multislot dresscode)
	(slot hasWifi)
	(slot latitude)
	(slot longitude))

;Startup
(defrule startup
	=>
	(assert (restaurant (name A) (isSmoker "yes") (minBudget 1000) (maxBudget 2000) (dresscode "casual") (hasWifi "yes") (latitude -6.8922186) (longitude 107.5886173)))
	(assert (restaurant (name B) (isSmoker "no") (minBudget 1200) (maxBudget 2500) (dresscode "informal") (hasWifi "yes") (latitude -6.224085) (longitude 106.7859815)))
	(assert (restaurant (name C) (isSmoker "yes") (minBudget 2000) (maxBudget 4000) (dresscode "formal") (hasWifi "no") (latitude -6.2145285) (longitude 106.8642591)))
	(assert (restaurant (name D) (isSmoker "no") (minBudget 500) (maxBudget 1400) (dresscode "formal") (hasWifi "no") (latitude -6.9005363) (longitude 107.6222191)))
	(assert (restaurant (name E) (isSmoker "yes") (minBudget 1000) (maxBudget 2000) (dresscode "casual" "informal") (hasWifi "yes") (latitude -6.2055617) (longitude 106.8001597)))
	(assert (restaurant (name F) (isSmoker "no") (minBudget 2500) (maxBudget 5000) (dresscode "informal") (hasWifi "yes") (latitude -6.9045679) (longitude 107.6399745)))
	(assert (restaurant (name G) (isSmoker "yes") (minBudget 1300) (maxBudget 3000) (dresscode "casual") (hasWifi "yes") (latitude -6.1881082) (longitude 106.7844409)))
	(assert (restaurant (name H) (isSmoker "no") (minBudget 400) (maxBudget 1000) (dresscode "inrofmal") (hasWifi "no") (latitude -6.9525133) (longitude 107.6052906)))
	(assert (restaurant (name I) (isSmoker "no") (minBudget 750) (maxBudget 2200) (dresscode "informal" "casual") (hasWifi "yes") (latitude -6.9586985) (longitude 107.7092281)))
	(assert (restaurant (name J) (isSmoker "no") (minBudget 1500) (maxBudget 2000) (dresscode "casual") (hasWifi "yes") (latitude -6.2769732) (longitude 106.775133)))
	(assert (get-name)))

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
		(assert (wifi ?input))))
