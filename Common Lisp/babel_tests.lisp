(progn
  (load 'prph)
  (load 'surf)
  (nonet)
;;  (net)
  ;; (trace do_frames do_heads_2 findheads popit token check_matching_syntax)
  ;; One half of Mary choked John...
  (EXPRESS '(((CON ((ACTOR (JOHN) <=> (*INGEST*) TO (*INSIDE* PART (JOHN)) FROM (*MOUTH* PART (JOHN)) OBJECT (*AIR*)) FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-2)) <≡ ((ACTOR (JOHN) <≡>T (*HEALTH* VAL (-10))) TIME (T-2))))))
  )

;; JCM new as of late 2020, generating a sentence with a 
;; playing with INGEST and AND
(EXPRESS '(((CON ((ACTOR (FAL) <=> (*INGEST*) OBJECT (BEER1 REF (DEF))) TIME (T-2)) ∧  ((ACTOR (HOST REF (INDEF)) <=> (*INGEST*) OBJECT (VIRUS REF (INDEF))) TIME (T-2))))))

;; playing with INGEST and AND with same actor
(EXPRESS '(((CON ((ACTOR (FAL) <=> (*INGEST*) OBJECT (BEER1 REF (DEF))) TIME (T-2)) ∧  ((ACTOR (FAL) <=> (*INGEST*) OBJECT (VIRUS REF (INDEF))) TIME (T-2))))))

(EXPRESS '(((ACTOR (HOST REF (INDEF)) <=> (*INGEST*) OBJECT (VIRUS REF (INDEF))) TIME (T-2))) )

;; Testing whether we can generate without an actor role.  Yes!  
(EXPRESS '((( <=> (*INGEST*) OBJECT (VIRUS)) TIME (T-2))) )

;; Demo 1. Mary choked John...
(EXPRESS '(((CON ((CON ((ACTOR (MARY) <=> (*GRASP*) OBJECT (*NECK* PART (JOHN))) TIME (T-4)) <≡ ((ACTOR (JOHN) <=> (*INGEST*) TO (*INSIDE* PART (JOHN)) FROM (*MOUTH* PART (JOHN)) OBJECT (*AIR*)) FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-2)))) ∧ ((CON ((ACTOR (JOHN) <=> (*INGEST*) TO (*INSIDE * PART (JOHN)) FROM (*MOUTH* PART (JOHN)) OBJECT (*AIR*)) FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-2)) <≡ ((ACTOR (JOHN) <≡>T (*HEALTH* VAL (-10))) TIME (T-2))))))))

;; Demo 2. Mary and John
(EXPRESS '(((CON
              ((CON
                 ((CON
                    ((ACTOR (MARY) <=> (*DO*)) TIME (T-1))
                    <≡
                    ((ACTOR (JOHN) <≡>T (*HEALTH* VAL (-10))) TIME (T-1))
                    ))
                 <≡C
                 ((ACTOR (MARY) <≡>T (*JOY*) ) INC(+3) TIME(T-1) )
                 ))
              <≡> (*MLOC* VAL (*LTM* PART (MARY))))
             TIME (T-2))
            )
  )


;;;;;;;;;;;;;;;;;;
;; TESTS OF Winnie Zheng's work to replace INGEST/EXPEL with PTRANS
;; Late 2020
;;;;;;;;;;;;;;;;;;

;; The "triple equals" in the double arrow makes simple present!
(EXPRESS '(((ACTOR (JOHN1) <≡> (*PTRANS*) OBJECT (JOHN1) TO (*OUTSIDE* PART(STORE REF (INDEF))) FROM(*INSIDE* PART(STORE REF (INDEF)))))))
;; Should produce:
;; (JOHN GOES TO STORES OUTSIDE) 
;; (JOHN COMES TO STORES OUTSIDE) 
;; (JOHN EXITS FROM A STORE) 
;; (JOHN LEAVES FROM A STORE) 

(EXPRESS '(((ACTOR (*ONE*) <≡> (*PTRANS*) OBJECT (KNIFE REF (INDEF)) TO (*INSIDE* PART(JOHN)) FROM(*SURFACE* PART(JOHN))))))
;; Should produce:
;; (JOHNS *INSIDE* IS GETING KNIFE FROM SOMEONE) 
;; (KNIFE IS COMING TO JOHNS *INSIDE*) 
;; (KNIFE IS ENTERING JOHNS *INSIDE*)
;; (KNIFE IS ENTERING JOHN)

(EXPRESS '(((ACTOR (*ONE*) <≡> (*PTRANS*) OBJECT (VIRUS REF(INDEF)) TO (*INSIDE* PART(HOST)) FROM(*SURFACE* PART(HOST))))))
;; Should produce:
;; (HOSTS *INSIDE* IS GETING A VIRUS FROM SOMEONE) 
;; (A VIRUS IS COMING TO HOSTS *INSIDE*) 
;; (A VIRUS IS ENTERING HOSTS *INSIDE*) 

(EXPRESS '(((ACTOR (*ONE*) <=> (*PTRANS*) OBJECT (WINE1) TO (CUP1 REF(INDEF)) FROM(PITCH1 REF(INDEF))))))
;; Should produce:
;; (A CUP IS GETING WINE FROM SOMEONE) 
;; (WINE IS COMING TO A CUP) 
;; (SOMEONE IS POURING WINE INTO A CUP FROM A PITCHER) 

(EXPRESS '(((ACTOR (*ONE*) <=> (*PTRANS*) OBJECT (WINE1) FROM ((PITCH1)) TO (CUP1)))))
;; Should produce:
;; (CUP IS GETING WINE FROM SOMEONE) 
;; (WINE IS COMING TO CUP) 
;; (WINE IS ENTERING CUP)

;; helpful traces
(trace ctyp mainlnkc)

;; These don't produce anything
(EXPRESS '((*INSIDE* PART(STORE REF (INDEF)))))
(EXPRESS '((JOHN1)))

;; These have equal sign "double arrows"
(EXPRESS '(((ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (*OUTSIDE* PART(STORE REF (INDEF))) FROM(*INSIDE* PART(STORE REF (INDEF)))))))
;; Should produce:
;; (JOHN IS GOING TO STORES *OUTSIDE*) 
;; (JOHN IS COMING TO STORES *OUTSIDE*) 
;; (JOHN IS EXITING FROM STORE) 
;; (JOHN IS LEAVING FROM STORE) 

;; Continuation of older examples...

;; Falstaff bought some wine from Hamlet
(EXPRESS '(((CON
             ((ACTOR (FAL) <=> (*ATRANS*) OBJECT (MONEY REF (INDEF)) TO (HAM)
                     FROM (FAL)) FOCUS ((ACTOR)) TIME (T-1))
             <≡≡>
             ((ACTOR (HAM) <=> (*ATRANS*) OBJECT (WINE1 REF (INDEF)) TO (FAL)
                     FROM (HAM)) TIME (T-1)FOCUS((ACTOR)))
              )FOCUS ((CON  OBJECT)))))

;; Some tracing
(trace do_frames do_heads findheads popit check_matching_syntax)
(untrace do_frames do_heads findheads popit)

;; trying to fix issues with quoted_head infinite loops
(trace qthd token putprop)
(untrace qthd token putprop)

;; These are good for fixing issues with FEXPRs
;; and discrimination nets
(trace aptree tnam smpval pval tval pot_head)
(trace doprp gochk gttree)

(untrace aptree tnam smpval pval tval pot_head)
(untrace doprp gochk gttree)

(trace id prop equ field)
;; don't print syntax nets.
(nonet)
;; print syntax nets.
(net)

(EXPRESS '(((ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (STORE2))) ))
(EXPRESS '(((ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (STORE2)) TIME (T-1) ) ))
(EXPRESS '(((ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (STORE2)) MODE ((*NEG*)) TIME (T-1) ) )) 
(EXPRESS '(((ACTOR (MARY1) <=> (*INGEST*) TO (*INSIDE* PART (MARY1)) FROM (*MOUTH* PART (MARY1))))))

(EXPRESS '(((ACTOR (BOOK1 REF (DEF)) <≡> (*POSS* VAL (MARY1)) TIME (T-1)))))
(EXPRESS '(((ACTOR (BOOK1 REF (DEF)) <≡> (*POSS* VAL (MARY1))))))

;; Othello choked Desdemona (FROM GARB.EX #5)
(EXPRESS '(((CON ((ACTOR (OTH) <=> (*GRASP*) OBJECT (*NECK* PART (DES))) TIME (T-1)) <≡ ((ACTOR (DES) <=> (*INGEST*) TO (*INSIDE* PART (DES)) FROM (*MOUTH* PART (DES)) OBJECT(*AIR*)) FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-1))))))

;; Another similar one
(EXPRESS '(((CON ((CON ((CON ((ACTOR (DES) <=> (*DO*)) TIME (T-1)) <≡ ((ACTOR (OTH) <≡>T (*HEALTH* VAL (-10))) TIME (T-1)))) <≡C ((ACTOR (DES) <≡>T (*JOY*) ) INC(+3) TIME(T-1) ))) <≡> (*MLOC* VAL (*LTM* PART (DES)))) TIME (T-2))))

; 2 John knew Bill wanted a book
(EXPRESS '(((CON ((CON ((CON ((ACTOR (*ONE1*) <=> (*ATRANS*) OBJECT (BOOK1 REF (INDEF)) TO (BILL1)) TIME (T-2)) <≡C ((ACTOR (BILL1) <≡>T (*JOY*) <≡>F (*JOY*)) INC (2) TIME (T-1)))) <≡> (*MLOC* VAL (*LTM* PART (BILL1)))) FOCUS ((<≡> VAL PART)) TIME (T-3)) <≡> (*MLOC* VAL (*LTM* PART (JOHN1)))) FOCUS  ((<≡> VAL PART))  TIME  (T-3))))

; 3 Hamlet advised Falstaff to drink the wine.
(EXPRESS '(((ACTOR (HAM) <=> (*MTRANS*) TO (*CP* PART (FAL)) FROM (*CP* PART (HAM))
	MOBJECT	((CON
		    ((ACTOR (FAL) <=> (*INGEST*) OBJECT (WINE1 REF (DEF))
		      TO (*INSIDE* PART (FAL)) FROM (*MOUTH* PART (FAL)))
		     TIME (T-2) FOCUS ((ACTOR)))
		  <≡C
		    ((ACTOR (FAL) <≡>T (*JOY*) <≡>F (*JOY*))
		     TIME (T-1) FOCUS ((ACTOR)) INC (2)))))
 TIME (T-3))))

; 4 John prevented Mary from reading the book
(EXPRESS '(((CON ((ACTOR (JOHN1) <=> (*DO*)) TIME (T-1))
      <≡
      ((ACTOR (MARY1) <=> (*MTRANS*) TO (*CP* PART (MARY1)) MOBJECT (*CONCEPTS*)
	      FROM(BOOK1 REF (DEF)))
       FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-1))
))))


; 6 Mary ceased to have the book
(EXPRESS '(((ACTOR (BOOK1 REF (DEF)) <≡> (*POSS* VAL (MARY1))) TIME (T-1))))

; 7 Bill will give (return) a book to Mary
(EXPRESS '(((ACTOR (BILL1) <=> (*ATRANS*) OBJECT (BOOK1 REF (DEF)) TO (MARY1) FROM (BILL1))
 FOCUS ((ACTOR)) TIME (T2))))

; 8 Mary became happy
(EXPRESS '(((ACTOR (MARY1) <≡>T (*JOY* ) <≡>F (*JOY*)) TIME (T-3) INC (2))))

; 9 Mary is dead
(EXPRESS '(((ACTOR (MARY1) <≡> (*HEALTH* VAL (-10))) TIME (T-0) )))

; 10 Falstaff bought some wine from Hamlet
(EXPRESS '(((CON
      ((ACTOR (FAL) <=> (*ATRANS*) OBJECT (MONEY REF (INDEF)) TO (HAM)
	FROM (FAL)) FOCUS ((ACTOR)) TIME (T-1))
  <≡≡>
      ((ACTOR (HAM) <=> (*ATRANS*) OBJECT (WINE1 REF (INDEF)) TO (FAL)
	FROM (HAM)) TIME (T-1)FOCUS((ACTOR)))
)FOCUS ((CON  OBJECT)))))


; This is the same AND structure that was used the generate the examples in Neil's  WRK/STR.TR on Othello choking Desdemona.  As of 11-24-2019 this generates a bug
(EXPRESS '(((CON ((CON ((ACTOR (OTH) <=> (*GRASP*) OBJECT (*NECK* PART (DES))) TIME (T-4)) <≡ ((ACTOR (DES) <=> (*INGEST*) TO (*INSIDE* PART (DES)) FROM (*MOUTH* PART (DES)) OBJECT (*AIR*)) FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-2)))) ∧ ((CON ((ACTOR (DES) <=> (*INGEST*) TO (*INSIDE * PART (DES)) FROM (*MOUTH* PART (DES)) OBJECT (*AIR*)) FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-2)) <≡ ((ACTOR (DES) <≡>T (*HEALTH* VAL (-10))) TIME (T-2))))))))

; OTHELLO CHOKED DESDEMONA
(EXPRESS '(((CON ((ACTOR (OTH) <=> (*GRASP*) OBJECT (*NECK* PART (DES))) TIME (T-4)) <≡ ((ACTOR (DES) <=> (*INGEST*) TO (*INSIDE* PART (DES)) FROM (*MOUTH* PART (DES)) OBJECT (*AIR*)) FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-2))))))

; OTHELLO GRABBED DESDEMONAS NECK
(EXPRESS '(((ACTOR (OTH) <=> (*GRASP*) OBJECT (*NECK* PART (DES))) TIME (T-4))))

;DESDEMONA COULD NOT BREATHE (on 12-24-2019 caused stack overflow, but I fixed it!)
(EXPRESS '(((ACTOR (DES) <=> (*INGEST*) TO (*INSIDE* PART (DES)) FROM (*MOUTH* PART (DES)) OBJECT (*AIR*)) FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-2))))

;"DESDEMONA BREATHED", "DESDEMONA INHALED AIR" (without *CANNOT*)
(EXPRESS '(((ACTOR (DES) <=> (*INGEST*) TO (*INSIDE* PART (DES)) FROM (*MOUTH* PART (DES)) OBJECT (*AIR*)) FOCUS ((ACTOR)) TIME (T-2))))

;; BILL POSSIBLY
(EXPRESS '(((CON ((CON
		((ACTOR (*ONE1*) <=> (*ATRANS*) OBJECT (BOOK1 REF (INDEF)) TO (BILL1))
		 TIME (T1))
	      <≡C
		((ACTOR (BILL1) <≡>T (*JOY*) <≡>F (*JOY*))
		 INC (2) TIME (T2))))
	<≡>
	    (*MLOC* VAL (*LTM* PART (BILL1))))
	FOCUS ((<≡> VAL PART)) TIME (T-0) CERTAINTY (.50))))

; JOHN MADE MARY READ THE BOOK
(EXPRESS '(((CON ((ACTOR (JOHN1) <=> (*DO*)) TIME (T-1))
      <≡
      ((ACTOR (MARY1) <=> (*MTRANS*) TO (*CP* PART (MARY1)) MOBJECT (*CONCEPTS*)
	      FROM(BOOK1 REF (DEF)))
       FOCUS ((ACTOR))  TIME (T-1))
))))

;; MARY READ THE BOOK
(EXPRESS '(((ACTOR (MARY1) <=> (*MTRANS*) TO (*CP* PART (MARY1)) MOBJECT (*CONCEPTS*)
	      FROM (BOOK1 REF (DEF)))
       FOCUS ((ACTOR))  TIME (T-1))))

(EXPRESS '(((ACTOR (JOHN1) <=> (*MTRANS*) TO (*CP* PART (MARY1) REF (DEF)) FROM (*CP* PART (JOHN1) REF (DEF)) MOBJECT ((CON ((ACTOR (BILL1) <=> (*DO*)) TIME (T-2) MODE (NIL)) <≡ ((ACTOR (MARY1) <≡>T (*HEALTH* VAL (-10)) <≡>F (*HEALTH* VAL (NIL))) MODE (NIL) TIME (T-2))))) TIME (T-3) FOCUS ((ACTOR)))))

(EXPRESS '(((ACTOR (JOHN1) <=> (*MTRANS*) TO (*CP* PART (MARY1)) FROM (*CP* PART (JOHN1)) MOBJECT (*CONCEPTS*))) TIME (T-1)))

(EXPRESS '(((ACTOR (JOHN1) <=> (*MTRANS*) TO (*CP* PART (MARY1)) FROM (BOOK1 REF (DEF)) MOBJECT (*CONCEPTS*)))
       FOCUS ((ACTOR))  TIME (T-1)))


(EXPRESS '((ACTOR (MARY1) <=> (*MTRANS*) TO (*CP* PART (MARY1)) MOBJECT (*CONCEPTS*)
	      FROM (BOOK1 REF (DEF)))))

(EXPRESS '(((ACTOR (JOHN1) <=> (*MTRANS*) TO (*CP* PART (MARY1)) MOBJECT (*CONCEPTS*))
       FOCUS ((ACTOR))  TIME (T-1))))

;; BILL KILLED MARY BY DOING SOMETHING
(EXPRESS '(((CON ((ACTOR (BILL1) <=> (*DO*)) TIME (T-2) MODE (NIL)) <≡ ((ACTOR (MARY1) <≡>T (*HEALTH* VAL (-10)) <≡>F (*HEALTH* VAL (NIL))) MODE (NIL) TIME (T-2))))))

(EXPRESS '(	((ACTOR (IAG) <=> (*MTRANS*) FROM (*CP* PART (IAG)) TO (*CP* PART (OTH))
	  MOBJECT ((ACTOR (HANDKERCHIEF *OWN* (DES)) <≡> (*POSS* VAL (CAS)))
	  	    TIME (T-3))
	  ) TIME (T-3))))



; Try to test the EKE tree?  Doesn't look like this should give us anything?  Wait ... why isn't the word "because" in here
(step (APTREE 'EKE
 '((CON ((ACTOR (JOHN1) <=> (*DO*)) TIME (T-1)) <≡
    ((ACTOR (MARY1) <=> (*MTRANS*) TO (*CP* PART (MARY1)) MOBJECT (*CONCEPTS*)
      FROM (BOOK1 REF (DEF)))
     FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-1))))))


; Try to test the EVT tree to see if I can get an UNABLE out of it
(step (APTREE 'EVT
 '((ACTOR (DES) <=> (*INGEST*) TO (*INSIDE* PART (DES)) FROM
    (*MOUTH* PART (DES)) OBJECT (*AIR*))
   FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-1))))


(FINDHEADS '((CON ((CON
		((ACTOR (*ONE1*) <=> (*ATRANS*) OBJECT (BOOK1 REF (INDEF)) TO (BILL1))
		 TIME (T1))
	      <≡C
		((ACTOR (BILL1) <≡>T (*JOY*) <≡>F (*JOY*))
		 INC (2) TIME (T2))))
	<≡>
	    (*MLOC* VAL (*LTM* PART (BILL1))))
	FOCUS ((<≡> VAL PART)) TIME (T-0) CERTAINTY (.50)))



(EXPRESS '(((ACTOR (BILL1) <=> (*ATRANS*) OBJECT (BOOK1 REF (DEF)) TO (MARY1) FROM (BILL1))
 FOCUS ((ACTOR)) TIME (T2))))

(EXPRESS '(((ACTOR	(HAM) <=> (*MTRANS*) TO (*CP* PART (FAL)) FROM (*CP* PART (HAM))
	MOBJECT	((CON
		    ((ACTOR (FAL) <=> (*INGEST*) OBJECT (WINE1 REF (DEF))
		      TO (*INSIDE* PART (FAL)) FROM (*MOUTH* PART (FAL)))
		     TIME (T-2) FOCUS ((ACTOR)))
		  <≡C
		    ((ACTOR (FAL) <≡>T (*JOY*) <≡>F (*JOY*))
		     TIME (T-1) FOCUS ((ACTOR)) INC (2)))))
 TIME (T-3))))




(EXPRESS '(((ACTOR	(HAM) <=> (*MTRANS*) TO (*CP* PART (FAL)) FROM (*CP* PART (HAM))
	MOBJECT	((CON
		    ((ACTOR (FAL) <=> (*INGEST*) OBJECT (WINE1 REF (DEF))
		      TO (*INSIDE* PART (FAL)) FROM (*MOUTH* PART (FAL)))
		     TIME (T-2) FOCUS ((ACTOR)))
		  <≡C
		    ((ACTOR (FAL) <≡>T (*JOY*) <≡>F (*JOY*))
		     TIME (T-1) FOCUS ((ACTOR)) INC (2)))))
 TIME (T-3))))


(EXPRESS '((((CON ((CON
		((ACTOR (*ONE1*) <=> (*ATRANS*) OBJECT (BOOK1 REF (INDEF)) TO (BILL1))
		 TIME (T1))
	      <≡C
		((ACTOR (BILL1) <≡>T (*JOY*) <≡>F (*JOY*))
		 INC (2) TIME (T2))))
	<≡>
	    (*MLOC* VAL (*LTM* PART (BILL1))))
	FOCUS ((<≡> VAL PART)) TIME (T-0) CERTAINTY (.50)))))

(setq CD '((ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (STORE2))))

(step (TNAM '((ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (STORE2)))))

;;;;;;;;;;;; ======================
;; Generation example files:
;;;;;;;;;;;; ======================

(load 'atrans.ex)
(load 'believ.ex) ;; these need work
(load 'crud.ex)
(load 'ekc.ex)
(load 'germ.ex)
(load 'go.ex) ;; These should probably be PTRANS
(load 'ingest.ex) ;; fourth one needs work
(load 'loan.ex) ;; Only one, but it works
(load 'mtrans.ex) ;; these need work
(load 'pay.ex) ;; first (and only) entry is good
(load 'perc.ex) ;; needs bugfixing
(load 'ptrans.ex) ;; these need a lot of work!
(load 'rand.ex) ;; all are OK
(load 'stat.ex) ;; all are OK
(load 'tst.ex) ;; first entry is OK but needs some fixing 
(load 'warn.ex) ; identical to tst.ex

;; How to run examples from these files after loading

(EXPRESS (list (nth 2 *ekc.ex*)))
(EXPRESS (list (nth 2 *atrans.ex*)))

;;;;;;;;;;;;
;; USEFUL CODE EXAMPLES FOR DEVELOPMENT AND DEBUGGING
;;;;;;;;;;;; =======================

;; How to generate text straight from surf.lisp
;; These were modified from my ATN code, so they have entries like
;; AUX which aren't used in BABEL's grammar.
;; This should generate "John saw"
(setf (symbol-plist 'E1) '(LEX (SEE) AUX ((1 2)) SUBJ E2 VOICE (ACT) FORM (SIM) TENSE (PAST) MOOD (INDIC)) )
(setf (symbol-plist 'E2) '(LEX (JOHN) NBR (S) TYP (SING3)) )


(INIT_SURF)
(gen 'E1)

;; Playing with "meta sentence" grammars
;; This should generate John saw and he saw
(progn
  (setf (symbol-plist 'E1) '(LEX (SEE) AUX ((1 2)) SUBJ E2 VOICE (ACT) FORM (SIM) TENSE (PAST) MOOD (INDIC)) )
  (setf (symbol-plist 'E2) '(LEX (JOHN) NBR (S) TYP (SING3)) )
  (setf (symbol-plist 'E3) '(LEX (SEE) AUX ((1 2)) SUBJ E2 VOICE (ACT) FORM (SIM) TENSE (PAST) MOOD (INDIC)) )
  (setf (symbol-plist 'E4) '(FIRS E1 LEX AND SECS E3))

  (INIT_SURF)
  (gen 'E4)
  )

;; Generating things like "John went and saw" without a subject in the second
;; clause.  To do this I added a direct transition in the verb phrase part of the grammar that skips
;; SUBJ, and just removed the SUBJ entry from E3 in the syntax net.  (actually, I changed it to
;; SUBJ-SKIPPED so that it's still there just in case.

(progn
  (setf (symbol-plist 'E1) '(LEX (GO) AUX ((1 2)) SUBJ E2 VOICE (ACT) FORM (SIM) TENSE (PAST) MOOD (INDIC)) )
  (setf (symbol-plist 'E2) '(LEX (JOHN) NBR (S) TYP (SING3)) )
  (setf (symbol-plist 'E3) '(LEX (SEE) AUX ((1 2)) SUBJ-SKIPPED E2 VOICE (ACT) FORM (SIM) TENSE (PAST) MOOD (INDIC)) )
  (setf (symbol-plist 'E4) '(FIRS E1 LEX AND SECS E3))

  (INIT_SURF)
  (gen 'E4)
  )

;; Playing with clause embedding

(progn
  (load 'prph)
  (load 'surf)
  (setf (symbol-plist 'E1) '(LEX (GO) SUBJ E3 VOICE (ACT) FORM (SIM) TENSE (PAST) MOOD (INDIC)) )
  (setf (symbol-plist 'E2) '(LEX (SEE) ACTSBJ (E2) VOICE (ACT) FORM (SIM) TENSE (PAST) MOOD (INDIC)) )
  (setf (symbol-plist 'E3) '(LEX (JOHN) ACTSBJ* (E2) NBR (S) TYP (SING3)) )
 
  (INIT_SURF)
  (gen 'E1)
  )


;; ========================================

;; Some other test code:
; ===
;; FINDHEADS puts the CD structure through the discrimination nets and
;; Returns the "heads": the symbols in the concexicon (CXCN) that matched
;; and could be generated.  For simpler structures (E.g. JOHN1), it just
;; returns the word corresponding to the structure.
;; Example below 

(FINDHEADS '((ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (STORE2))) )

;; Should return something like
;; => 
;; ((GO1 (ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (STORE2)))
;;  (COME1 (ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (STORE2)))
;;  (ENTER1 (ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (STORE2)))
;;  (ENTER2 (ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (STORE2))))

(FINDHEADS '((JOHN1)))

;; Should return
;; => ((JOHN1))

; ===
;; Given a CD structure, TNAM returns the names of the matching discrimination
;; nets (minus the .TRE suffix in their filenames).  FINDHEADS uses this to get
;; the matching nets/trees.  Example below

(TNAM '((CON
    ((ACTOR (OTH) <=> (*GRASP*) OBJECT (*NECK* PART (DES))) TIME (T-0))
    <≡ 
    ((ACTOR (DES) <=> (*INGEST*) TO (*INSIDE* PART (DES)) FROM (*MOUTH* PART (DES))
	  OBJECT(*AIR*))
     FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-0))
           )))

;; This should return something like
;; => (EKE KAUS)
;; Indicating that the EKE "Event Cause (Kause) Event" and KAUS "cause"
;; D-nets match.  Mainly this determination is done with the "main link" ( <≡ )


; ===
;; APTREE = "apply tree" applies the particular D-net to the CD structure
;; FINDHEADS uses this to get the "heads" for any particular tree (if any)

(APTREE 'EKE '((CON ((ACTOR (OTH) <=> (*GRASP*) OBJECT (*NECK* PART (DES))) TIME (T-0)) <≡ ((ACTOR (DES) <=> (*INGEST*) TO (*INSIDE* PART (DES)) FROM (*MOUTH* PART (DES)) OBJECT (*AIR*)) FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-0)))))

;; In this case it returns something like
;; => 
;; ((CHOKE1
;;   (CON ((ACTOR (OTH) <=> (*GRASP*) OBJECT (*NECK* PART (DES))) TIME (T-0)) <≡
;;    ((ACTOR (DES) <=> (*INGEST*) TO (*INSIDE* PART (DES)) FROM
;;      (*MOUTH* PART (DES)) OBJECT (*AIR*))
;;     FOCUS ((ACTOR)) MODE ((*CANNOT*)) TIME (T-0)))))

;; Another example.  This one returns multiple matches:

(APTREE 'PTRANS '((ACTOR (JOHN1) <=> (*PTRANS*) OBJECT (JOHN1) TO (STORE2))) )

; ===
;; TREEIN reads in the discrimination net "tree" file (AND.TRE in this case)
(TREEIN 'AND)


;; ===
;; Testing predicates

(DEFUN PRED_TEST (PRED CD)
  (FUNCALL (CAR PRED) (LIST CD (FIELD (CADR PRED) CD) (THIRD PRED)))
  )

(PRED_TEST
  '(ID (CON ACTOR) (∧ ACTOR))
  '((CON ((ACTOR (FAL) <=> (*INGEST*) OBJECT (BEER1))) ∧ ((ACTOR (FAL) <=> (*INGEST*) OBJECT (VIRUS)))))
  )
)

;; Testing DELETIONS.  I can "surgically" delete a role inside of a complex CD structure
;; For example
(SETQ TESTCD '((CON ((ACTOR (FAL) <=> (*INGEST*) OBJECT (VIRUS)) TIME (T-2)))))
(DELETIONS '((CON ACTOR)) TESTCD NIL)
;; Check out the value of TESTCD after calling DELETIONS.  it should be
;; => ((CON ((NIL NIL <=> (*INGEST*) OBJECT (VIRUS)) TIME (T-2))))
;; NIL is OK for the third argument, since deletions doesn't do anything
;; with syntax net nodes.

;; A simple example.  Should generate "INGESTED VIRUS" without subject
(SETQ TESTCD2 '((ACTOR (FAL) <=> (*INGEST*) OBJECT (VIRUS)) TIME (T-2)))
(DELETIONS '((ACTOR)) TESTCD2 NIL)
(EXPRESS (LIST TESTCD2))

; other random stuff

  ;; (SYMBOL-PLIST 'E1)
  ;; (GET 'NP 'AFSTN)
  ;; (GET 'SING3 'GO)
  ;; (equal #\C (first (last (EXPLODE 'ABC))))
  ;; (intern (concatenate 'string "A" "B"))
