;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                          
;;;;  Micro SAM
;;;;
;;;;  Common Lisp implementation by:
;;;;
;;;;  Bill Andersen (waander@cs.umd.edu),
;;;;  Department of Computer Science
;;;;  University of Maryland
;;;;  College Park, MD  20742
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :cd-functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Global Variables
;;;

(defvar *active-scripts* nil
  "List of currently active scripts. Each item 
is a data structure for a script containing
script-name, possible-next-events, bindings.")

(defvar *data-base* nil
  "Pointer to CD database")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Top Level Functions
;;;

(defun process-story (story)
  "Takes a story (just a list of CDs) and passes each to
PROCESS-CD.  At the end of the story, the current script
is added to the database and the database is pretty-printed."
  (clear-scripts)      ; Reset global variables
  (user-trace "~%")   
  (dolist (cd story)   ; For every CD in story, which is a list of CDs,
    (user-trace "~&Input is") (print-cd cd) ; print the CD.
    (process-cd cd) ; Attempt to either integrate the CD
		    ; with one of the active scripts or search it for
		    ; a new script, and add it to the database.
    (user-trace "~%"))
  (user-trace "~%Story done - final script header") ; Once we've gone through every CD in the story,
  (print-cd (add-cd *active-scripts*)) ; add active-scripts to the database.
  (user-trace "~%Database contains:")
  (print-cd *data-base*))		; Print the database

(defun process-cd (cd)
  "Takes one CD of the story at a time.  Either a
statement is predicted by the current script or it
is in the database or it suggests a new script."
  (or (integrate-into-multi-scripts cd) ; First, attempt to find the first event
					; in one of the currently active scripts
					; that matches the input cd, and add it to the database.
      (suggest-new-script cd) ; If there are no matches in active-scripts, instead search
			      ; the input cd for a reference to a new script, and add it
			      ; to the database.
      (progn (user-trace "~%Adding") ; If cd doesn't have any script references, 
             (print-cd (add-cd cd))  ; add it to the database anyway
             (user-trace "~%- not linked to any script")))) ; and print that it is not linked to a script

(defun clear-scripts ()
  "Resets all globals to NIL"
  (setf *data-base* nil) 
  (setf *active-scripts* nil))

(defun add-cd (cd)
  "Adds a new CD to the end of the DB list"
  (setf *data-base* (cons-end *data-base* cd)) ; Global variable *data-base* is updated to include the input cd
  cd)					       ; Return cd

(defun get-possible-next-events (script)
  "Gets the list of possible next events associated with 
the input script. Temporally ordered list of events which
have not been seen in the input yet."
  (assoc 'possible-next-events script)) ; Gets the form (POSSIBLE-NEXT-EVENTS (A) (B) (C) ... (N))

(defun get-bindings (script)
  "Gets the list of bindings associated with the
input script."
  (assoc 'bindings script)) ; Gets the form (BINDINGS NIL (A) (B) (C) ... (N))

(defun integrate-into-script (cd script)
  "Attempts to integrate an incoming statement into the 
currently active script, by finding the first event in 
the input script's possible-next-events that matches the 
statement. If such an event is found, update the database."
  (do* ((new-bindings nil) ; Loop variable new-bindings is initialized to nil
        (events (cadr (get-possible-next-events script))
		(cdr events)) ; Loop variable events is initialized to
			      ; the list of possible next events of script.
			      ; Each time the loop runs, events is updated
			      ; to cdr events (taking off the first item).
        (event (first events)
	       (first events))) ; Loop variable event is initialized to first
				; item in events. Each time the loop runs,
				; event is updated to first item of events
				; based on the update from the previous line.
      ;; Exit clause
      ((or (null event) new-bindings) ; If event is null or new-bindings is non-nil, 
       new-bindings)	       ; exit the loop and return new-bindings.
    ;; Loop body
    (setq new-bindings (match event cd (cdr (get-bindings script)))) ; See if the input CD matches the pattern
								     ; of the event variable based on the bindings
								     ; of the input script. If a match,
								     ; return T (var val)*, otherwise return NIL.
    (when new-bindings			; If new-bindings is non-nil,
      (rplacd (get-bindings script) new-bindings) ; update the input script's bindings to new-bindings.
      (user-trace "~%Matches")
      (print-cd event)
      (add-script-info event script)))) ; Fill in missing events from script up to this event in the story

(defun integrate-into-multi-scripts (cd)
  "Loops over the list of active scripts and calls
integrate-into-script on each script."
  (let (retval)							     ; Initialize local variable retval.
    (dolist (script *active-scripts* retval)			     ; For every script in *active-scripts*,
      (if (integrate-into-script cd script)			     ; call integrate-into-script. If returns non-nil,
	  (setf retval T)))))					     ; set retval to T (returns T). Otherwise, return nil.

(defun check-duplicate (cd)
  "Helper function for ADD-SCRIPT-INFO that checks an
incoming CD for a match with any CDs currently in the
database. If there is a match, then exit function and
return nil (F). If there isn't, return T."
  (setq bindings nil) ; define variable - we just need this to be able to call the match function
  (setq matched nil) ; define variable - this will be either T or F after the loop
  (dolist (event *data-base*) ; loop over cds in database
    (setq matched (match event cd bindings)) ; check for a match between input cd and cd in database
    (if matched ; if there is a match, that means this input cd already exists in database
	(return-from check-duplicate t))) ; exit function and return t
  (return-from check-duplicate nil)) ; if after looping over cds in database there is no match, return nil 

(defun add-script-info (position script)
  "ADD-SCRIPT-INFO is given an event in a script (the 
one that matched the input in INTEGRATE-INTO-SCRIPT).
Each script event up through position is instantiated
and added to the database."
  (do* ((events (cadr (get-possible-next-events script))
		(cdr events)) ; Loop variable events is initialized to
			      ; the list of possible next events of script.
			      ; Each time the loop runs, events is updated
			      ; to cdr events (taking off the first item).
        (event (first events)
	       (first events)) ; Loop variable events is initialized to first
			       ; item in events. Each time the loop runs,
			       ; event is updated to first item of events
			       ; based on the update from the previous line.
        (at-position-p nil)) ; Loop variable at-position-p is initialized to nil.
      ;; Exit clause
      ((or (null event) at-position-p) ; If event is null (we looped over all items in events)
				       ; or at-position-p is T (meaning event == at-position-p,
				       ; aka we've looped over the events up to the input),
       (setf (cadr (get-possible-next-events script)) events)) ; update input script's possible next events to
							       ; events, which contains all the script events
							       ; after the input cd, and return & exit the loop.
    ;; Loop body
    (user-trace "~%Adding script CD")
    (setq instantiated-cd (instantiate event (cdr (get-bindings script)))) ; Instantiate event (replace its variables
									; with appropriate bindings from *active-scripts*)
    (if (equal (check-duplicate instantiated-cd) nil) ; Check if instantiated event matches any events in database.
	(print-cd (add-cd instantiated-cd))) ; If F, then proceed to add-cd. Otherwise if T, then do not add.
    (setq at-position-p (equal event position)))) ; Set at-position-p to the value of whether event
					; is equal to position (the input event).

(defun add-script-info-og (position script)
  "ADD-SCRIPT-INFO is given an event in a script (the 
one that matched the input in INTEGRATE-INTO-SCRIPT).
Each script event up through position is instantiated
and added to the database."
  (do* ((events (cadr (get-possible-next-events script))
		(cdr events)) ; Loop variable events is initialized to
			      ; the list of possible next events of script.
			      ; Each time the loop runs, events is updated
			      ; to cdr events (taking off the first item).
        (event (first events)
	       (first events)) ; Loop variable events is initialized to first
			       ; item in events. Each time the loop runs,
			       ; event is updated to first item of events
			       ; based on the update from the previous line.
        (at-position-p nil)) ; Loop variable at-position-p is initialized to nil.
      ;; Exit clause
      ((or (null event) at-position-p) ; If event is null (we looped over all items in events)
				       ; or at-position-p is T (meaning event == at-position-p,
				       ; aka we've looped over the events up to the input),
       (setf (cadr (get-possible-next-events script)) events)) ; update input script's possible next events to
							       ; events, which contains all the script events
							       ; after the input cd, and return & exit the loop.
    ;; Loop body
    (user-trace "~%Adding script CD")
    (print-cd (add-cd (instantiate event (cdr (get-bindings script))))) ; Instantiate event (replace its variables
									; with appropriate bindings from *active-scripts*)
									; and add it to the database.
    (setq at-position-p (equal event position)))) ; Set at-position-p to the value of whether event
					; is equal to position (the input event).

;; JCM part of the script combinator hack.  A new function to invoke a script so that I can call it before
;; we even start to process-cd.
(defun invoke-script (new-script)
  (user-trace "~%New script ~s" new-script) ; tracing
  (setf *active-scripts* (cons (list (list 'script-name new-script)
				   (list 'possible-next-events (events-script new-script))
				   (list 'bindings nil)) *active-scripts*))) ; Create a data structure for a new script
									     ; and add it to  *active-scripts*.

(defun suggest-new-script (cd)
  "Takes a CD form, adds it to the database, and checks 
the predicates of the form and its subforms until a link
to a script is found (if any).  Thus, in (PTRANS (ACTOR 
(PERSON)) (OBJECT (PERSON)) (TO (STORE))), the first 
script is found under STORE.  If there was a previous 
script, add it to the database before switching to another
script, but do not instantiate any events that were left 
in *possible-next-events*."
  (let ((new-script (find-script cd))) ; Attempt to find a script reference in the input cd,
				       ; and if one is found, local  variable new-script is
				       ; initialized to the referenced script.
    ;; JCM this now calls invoke script.  See definition above.
    (when new-script
      (invoke-script new-script) ; If there is a script reference, invoke it. 
      (integrate-into-multi-scripts cd)))) ; Call integrate-into-multi-scripts
					   ; to find the first instance of cd
					   ; in one of the active scripts and
					   ; fill in any missing previous events

(defun find-script (cd)
  (cond ((atom cd) (associated-script cd)) ; If input cd is an atom (a single symbol),
					   ; call associated-script on it to get the
					   ; corresponding script
        (t (or (associated-script (header-cd cd)) ; If cd is not an atom, get the predicate
						  ; of cd, and now try  getting the
						  ; corresponding script.
               (let ((script nil)) ; If still not script, initialize local variable script to nil.
                 (some #'(lambda (pair) ; For every pair in the list of role-value pairs
					; contained in cd,
                           (setf script (find-script (filler-pair pair)))) ; Get the filler of the pair,
									   ; recursively call find-script on it,
									   ; and assign script to the result.
                       (roles-cd cd)) 
                 script))))) ; Return script

(defun get-first-cd (script)
  "Helper function to grab first CD in a script."
  (car (events-script script)))

(defun get-to (cd)
  "Helper function to grab filler for TO in a PTRANS CD."
  (car (filler-role 'to (roles-cd cd))))

(defun find-script-new (cd)
  "New find-script function that instead calls the match function
on the input story CD and on the (first) CD of each script in
the system's corpus until there is a match in CD structure.
The script in which there is a match is activated."
  (setq bindings nil)
  (dolist (script *scripts*)
    (setq events (events-script script))
    (dolist (event events)
      (when (match event cd bindings)
	(setf active-script script)
	(return-from find-script-new active-script)))))

(defun print-cd (cd)
  "Pretty-prints a CD form indented 4 spaces."
  (user-trace "~&~4T~s~%" cd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Data Structures for Scripts
;;;

;; Script names are atoms with the EVENTS property 
;; of the atom pointing to a list of events.

(defun events-script (x)
  "Returns sequence of events stored under a script."
  (get x :events))

(defsetf events-script (script) (events)
  `(setf (get ,script :events) ,events))

(defun associated-script (x)
  "Returns script associated with predicate x."
  (get x :associated-script))

(defsetf associated-script (x) (script)
  `(setf (get ,x :associated-script) ,script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Scripts
;;;


;; An array for scripts in our corpus
(setf *scripts* '($shopping
		  $restaurant
		  $birthday))

;; $SHOPPING script
(setf (events-script '$SHOPPING)
      '((ptrans (actor ?shopper) (object ?shopper) (to ?store))
        (ptrans (actor ?shopper) (object ?bought) (to ?shopper))
        (atrans (actor ?store) (object ?bought)  (from ?store) (to ?shopper))
        (atrans (actor ?shopper) (object (money)) (from ?shopper) (to ?store))
        (ptrans (actor ?shopper) (object ?shopper) (from ?store) (to ?elsewhere))))
(setf (associated-script 'store) '$shopping)

;; $RESTAURANT script, short version
(setf (events-script '$restaurant)
      '((ptrans (actor ?diner) (object ?diner) (to ?restaurant)) 
        (mtrans (actor ?diner) (object (ingest (actor ?diner) (object ?meal))))
        (ingest (actor ?diner) (object ?meal))
        (atrans (actor ?diner) (object (money)) (from ?diner) (to ?restaurant))
        (ptrans (actor ?diner) (object ?diner) (from ?restaurant) (to ?elsewhere))))
(setf (associated-script 'restaurant) '$restaurant)

;; $BIRTHDAY script, short version
(setf (events-script '$birthday)
      '((ptrans (actor ?celebrant) (object ?celebrant) (to ?venue))
	(ptrans (actor ?guest) (object ?guest) (to ?venue))
	(mtrans (actor ?guest) (object (greeting)) (to ?celebrant))
	(atrans (actor ?guest) (object (cake)) (to ?celebrant))))
(setf (associated-script 'cake) '$birthday)

;; $RESTAURANT script, long version
;; (setf (events-script '$restaurant)
;;       '((ptrans (actor ?diner) (object ?diner) (to ?restaurant))
;; 	(ptrans (actor ?diner) (object ?diner) (to (table)))
;; 	(ptrans (actor ?waiter) (object ?waiter) (to (table)))
;; 	(atrans (actor ?waiter) (object (menu)) (to ?diner))
;; 	(mbuild (actor ?diner) (object (ingest (actor ?diner) (object ?food))))
;; 	(mtrans (actor ?diner)
;; 	 (object (atrans (actor ?waiter) (object ?food) (to ?diner)))
;; 	 (to ?waiter))
;; 	(ptrans (actor ?waiter) (object ?waiter) (to ?cook))
;; 	(mtrans (actor ?waiter)
;; 	 (object (atrans (actor ?waiter) (object ?food) (to ?diner)))
;; 	 (to ?cook))
;; 	(atrans (actor ?cook) (object ?food) (to ?waiter))
;; 	(atrans (actor ?waiter) (object ?food) (to ?diner))
;; 	(ingest (actor ?diner) (object ?food))
;; 	(atrans (actor ?waiter) (object check) (to ?diner))
;; 	(atrans (actor ?diner) (object money) (to ?waiter))
;; 	(ptrans (actor ?diner) (object ?diner) (to ?elsewhere))))
;; (setf (associated-script 'restaurant) '$restaurant)

;; $BIRTHDAY script, long version
;; (setf (events-script '$birthday)
;;       '((ptrans (actor ?celebrant) (object ?celebrant) (to ?venue))
;; 	(ptrans (actor ?guest) (object ?guest) (to ?venue))
;; 	(mtrans (actor ?guest) (object (greeting)) (to ?celebrant))
;; 	(atrans (actor ?guest) (object ?present) (to ?celebrant))
;; 	(ptrans (actor ?guest) (object ?guest) (to cake))
;; 	(grasp (actor ?guest) (object (cake)) (to (hand)))
;; 	(ptrans (actor ?guest) (object ?guest) (to ?celebrant))
;; 	(atrans (actor ?guest) (object (cake)) (to ?celebrant))
;; 	(mtrans (actor ?guest) (object (song)) (to ?celebrant))
;; 	(expel (actor ?celebrant) (object (air)) (from (mouth)) (to (candle)))
;; 	(ingest (actor ?celebrant) (object (cake)))
;; 	(ptrans (actor ?guest) (object ?guest) (to ?elsewhere))
;; 	(ptrans (actor ?celebrant) (object ?celebrant) (to ?elsewhere))))
;; (setf (associated-script 'cake) '$birthday)

;; $CHARGE script
(setf (events-script '$charge)
      '((grasp (actor ?driver) (object (hand)) (to (plug)))
	(ptrans (actor ?driver) (object (plug)) (to (outlet)))
	(ptrans (actor (electricity)) (object (electricity)) (from (station)) (to (car)))
	(ptrans (actor ?driver) (object (plug)) (from (outlet)))))
(setf (associated-script 'plug) '$charge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Stories
;;;

;; SHOPPING story, previously named kite-story
(defparameter shopping-story
      '((ptrans (actor (person (name (jack))))
                (object (person (name (jack))))
                (to (store)))
        (atrans (object (kite))
                (to (person)))
        (ptrans (actor (person))
                (object (person))
         (to (house)))))

;; RESTAURANT (SHORT) story
(defparameter restaurant-story
  '((ptrans (actor (person (name (jack))))
                (object (person (name (jack))))
                (to (restaurant)))
        (ingest (actor (person))
                (object (lobster)))
        (ptrans (actor (person))
                (object (person))
                (from (restaurant)))))

;; RESTAURANT BIRTHDAY (SHORT) story
(defparameter restaurant-birthday-short
  '((ptrans (actor (person (name (alexis))))
     (object (person (name (alexis))))
     (to (restaurant (name (applebees)))))
    (ingest (actor (person (name (alexis))))
     (object (steak)))
    (atrans (actor (waiter))
     (object (cake))
     (to (person (name (alexis)))))
    (ptrans (actor (person (name (alexis))))
     (object (person (name (alexis))))
     (from (restaurant (name (applebees)))))))

;; RESTAURANT BIRTHDAY (LONG) story
(defparameter restaurant-birthday-long
  '((ptrans (actor (person (name (alexis))))
     (object (person (name (alexis))))
     (to (restaurant (name (applebees)))))
    (atrans (actor (waiter))
     (object (menu))
     (to (person (name (alexis)))))
    (mtrans (actor (person (name (alexis))))
     (object (atrans (actor (waiter)) (object (steak)) (to (person (name (alexis))))))
     (to (waiter)))
    (atrans (actor (waiter)) (object (steak))
     (to (person (name (alexis)))))
    (ingest (actor (person (name (alexis))))
     (object (steak)))
    (atrans (actor (waiter))
     (object (cake))
     (to (person (name (alexis)))))
    (ingest (actor (person (name (alexis))))
     (object (cake)))
    (atrans (actor (waiter))
     (object (check))
     (to (person (name (alexis)))))
    (atrans (actor (person (name (alexis))))
     (object (money))
     (to (waiter)))))

;; CHARGE-SHOPPING story 
(defparameter charge-shopping
  '((ptrans (actor (jane)) (object (jane)) (to (store)))
    (ptrans (actor (jane)) (object (plug)) (to (outlet)))
    (atrans (actor (store)) (object (snack)) (from (store)) (to (jane))) 
    (ptrans (actor (jane)) (object (jane)) (from (store)))
    (ptrans (actor (jane)) (object (plug)) (from (outlet)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Revisiting MicroSAM
;;;  The Museum Script
;;;
;;;  Implementation of a museum script by Alexis Kilayko.

;; MUSEUM STORY 
;; (defparameter museum-story
;; 	'((ptrans (actor (person (name (libby))))
;; 				(object (person (name (libby))))
;; 				(to (museum (name (metropolitan)))))
;; 		(ptrans (actor (person (name (libby))))
;; 				(object (person (name (libby))))
;; 				(to (exhibit)))
;; 		(grasp (actor (person (name (libby)))) (object (camera)) (to (hand)))
;; 		(ptrans (actor (person (name (libby)))) 
;; 				(object (person (name (libby)))) 
;; 				(to (store)))
;; 		(atrans (object (souvenir)) (to (person (name (libby)))))
;; 		(ptrans (actor (person (name (libby)))) 
;; 				(object (person (name (libby)))) 
;; 				(to (cafe)))
;; 		(atrans (object (cookie)) (to (person (name (libby)))))
;; 		(ptrans (actor (person (name (libby))))
;; 				(object (person (name (libby))))
;; 				(to (outside)))))

;; $MUSEUM SCRIPT
;; (setf (events-script '$museum)
;; 	'((ptrans (actor ?customer) (object ?customer) (to ?museum))
;; 	    (mtrans (actor ?customer)
;; 	            (object (atrans (actor ?customer)
;; 	                            (object (ticket))
;; 								(from (clerk))
;; 								(to ?customer)))
;; 				(from ?customer)
;; 				(to (clerk)))
;; 		(atrans (actor ?customer) (object (money)) (from ?customer) (to (clerk)))
;; 		(atrans (actor (clerk)) (object (ticket)) (from (clerk)) (to ?customer))
;; 		(atrans (actor (clerk)) (object (map)) (from (clerk)) (to ?customer))
;; 		(ptrans (actor ?customer) (object ?customer) (to ?exhibit))
;; 		(atrans (actor ?customer) (object (ticket)) (from ?customer) (to (usher)))
;; 		(propel (actor (usher)) (object (ticket)))
;; 		(atrans (actor (usher)) (object (ticket)) (from (usher)) (to ?customer))
;; 		(attend (actor ?customer) (object (eye)) (to (art)))
;; 		(ptrans (actor ?customer) (object ?customer) (from ?museum) (to (outside)))))
;; (setf (associated-script 'museum) '$museum)

;; SUBSCRIPTS FOR $MUSEUM
;; $PHOTO SCRIPT
;; (setf (events-script '$photo)
;; 	'((move (actor ?customer) (object (hand)) (to (camera)))
;; 		(grasp (actor ?customer) (object (camera)) (to (hand)))
;; 		(move (actor ?customer) (object (hand)) (to (viewfinder)))
;; 		(propel (actor ?customer) (object (button)))))
;; (setf (associated-script 'camera) '$photo)

;; ;; $STORE SCRIPT
;; (setf (events-script '$store)
;; 	'((ptrans (actor ?customer) (object ?customer) (to ?store))
;; 		(attend (actor ?customer) (object (eye)) (to ?bought))
;; 		(grasp (actor ?customer) (object ?bought) (to (hand)))
;;         (mbuild (actor ?customer) ; ***
;;                 (object (atrans (actor ?customer)
;;                                 (object ?bought)
;; 								(from ?store)
;; 								(to ?customer)))
;; 				(from (ltm))
;; 				(to (cp)))
;; 		(ptrans (actor ?customer) (object ?customer) (to (cashier)))
;;         (mtrans (actor ?customer) ; ***
;; 	            (object (atrans (actor ?customer)
;; 	                            (object ?bought)
;; 								(from (cashier))
;; 								(to ?customer)))
;; 				(from ?customer)
;; 				(to (cashier)))
;; 		(atrans (actor ?customer) (object (money)) (from ?customer) (to (cashier)))
;; 		(move (actor (cashier)) (object (hand)) (to ?bought))
;; 		(atrans (actor (cashier)) (object ?bought) (from cashier) (to ?customer))))
;; (setf (associated-script 'store) '$store)

;; ;; $CAFE SCRIPT
;; (setf (events-script '$cafe)
;; 	'((ptrans (actor ?customer) (object ?customer) (to ?cafe))
;; 		(move (actor ?customer) (object (hand)) (to (menu)))
;;         (mbuild (actor ?customer)
;; 	            (object (ingest (actor ?customer)
;; 	                            (object ?food)))
;; 				(from (ltm))
;; 				(to (cp)))
;;         (mtrans (actor ?customer)
;;                 (object (ingest (actor ?customer)
;;                                 (object ?food)))
;; 				(from ?customer)
;; 				(to (barista)))
;; 		(atrans (actor ?customer) (object (money)) (from ?customer) (to (barista)))
;; 		(atrans (actor (barista)) (object ?food) (from (barista)) (to ?customer))))
;; (setf (associated-script 'cafe) '$cafe)
