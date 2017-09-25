; Menerima Input Nama
(defrule get-name
	=>
	(printout t "What is your name? ")
	(bind ?input (readline))
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
