(defun cd-header-translate (header)
  "Translates just the ELI/SAM primitive into a version preferred by BABEL"
  (case header
    ((ptrans) '*ptrans*)
    ((grasp) '*grasp*)
    ((atrans) '*atrans*)
    ;; etc. need to add the rest, or a more elegant solution
    ))

(defun cd-translate (cd)
  "Translates an ELI CD form into a BABEL CD form."
  (list (list (remove 'NIL (list 'ACTOR (filler-role 'ACTOR cd)
                           
                                 '<≡> (list (cd-header-translate (header-cd cd)))
          
                                 (when (filler-role 'OBJECT cd) 'OBJECT)
                                 (when (filler-role 'OBJECT cd) (filler-role 'OBJECT cd))
                     
                                 (when (filler-role 'FROM cd) 'FROM)
                                 (when (filler-role 'FROM cd) (filler-role 'FROM cd))
          
                                 (when (filler-role 'THRU cd) 'THRU)
                                 (when (filler-role 'THRU cd) (filler-role 'THRU cd))
                     
                                 (when (filler-role 'TO cd) 'TO)
                                 (when (filler-role 'TO cd) (filler-role 'TO cd)))))))

;; some tests
;; (cd-translate '(PTRANS (ACTOR (JANE)) (OBJECT (JANE)) (TO (STORE))))
;; (cd-header-translate 'ptrans)
;; (header-cd '(PTRANS (ACTOR (JANE)) (OBJECT (JANE)) (TO (STORE))))

;; (express '(((ACTOR (JANE) <=> (PTRANS) OBJECT (JANE) TO (STORE)))))  ;; This raises a CAR: 0 is not a list
;; (express '(((ACTOR (JANE) <=> (*PTRANS*) OBJECT (JANE) TO (STORE)))))
;; (express '(((ACTOR (JANE) <=> (*GRASP*) OBJECT (HAND) TO (PLUG)))))

;; (express (cd-translate '(PTRANS (ACTOR (JANE)) (OBJECT (JANE)) (TO (STORE)))))
;; (express (cd-translate '(GRASP (ACTOR (JANE)) (OBJECT (HAND)) (TO (PLUG)))))

;; example from the script combinator
;; (mapcar #'(lambda (x) (express (cd-translate x)))
;;   '((PTRANS (ACTOR (JANE)) (OBJECT (JANE)) (TO (STORE)))
;;    (GRASP (ACTOR (JANE)) (OBJECT (HAND)) (TO (PLUG)))
;;    (PTRANS (ACTOR (JANE)) (OBJECT (PLUG)) (TO (OUTLET)))
;;    (PTRANS (ACTOR (JANE)) (OBJECT (SNACK)) (TO (JANE)))
;;    (ATRANS (ACTOR (STORE)) (OBJECT (SNACK)) (FROM (STORE)) (TO (JANE)))
;;    (ATRANS (ACTOR (JANE)) (OBJECT (MONEY)) (FROM (JANE)) (TO (STORE)))
;;    (PTRANS (ACTOR (JANE)) (OBJECT (JANE)) (FROM (STORE)) (TO NIL))
;;    (PTRANS (ACTOR (ELECTRICITY)) (OBJECT (ELECTRICITY)) (FROM (STATION)))))

