;; JCM: Should we put earmuffs on all of these?
(DEFPARAMETER !GR 'AFSTN)
(DEFPARAMETER !BREAKING T)
(DEFPARAMETER !TYP T)
(DEFPARAMETER !TMP! T)
(DEFPARAMETER !NODE T)


(DEFUN GEN (NODE)
  (PROG2 (PUTPROP NODE 'S 'LAB) (PRINT (FINDPATHS NODE))))


(DEFUN FINDPATHS (NODE)
  (PROG2 (COND (!BREAKING (BREAK NIL))) (CHOOSEPATH NODE (GET (GET NODE 'LAB) !GR))))


(DEFUN CHOOSEPATH (NODE RHS)
  (PROG (RES)
        (PROG (&V &L1 ALT)
              (SETQ &L1 RHS)
         LOOP (COND ((NULL &L1) (RETURN &V)))
              (SETQ ALT (CAR &L1))
              (SETQ &L1 (CDR &L1))
              (COND
               ((SETQ RES
                  (COND
                    ;; An ATOM means a "free" transition, no arc symbol
                    ;; Usually it's the last in the RHS list
                    ;; Go ahead and take the transition and start finding paths
                    ((ATOM ALT) (PUTPROP NODE ALT 'LAB) (FINDPATHS NODE))
                    ;; If the node has the arc symbol or the arc symbol is a DF, follow the path
                    ;; A DF arc should be at the end of the list, or the only in the list
                    ; %traverse free path, no actions%
                    ((OR (GET NODE (CAR ALT)) (GET (CAR ALT) 'DF)) (FOLLOWPATH NODE ALT))))
                (RETURN &V)))
              (GO LOOP))
        (RETURN RES)))


(DEFUN FOLLOWPATH
  (NODE GRUL)
  (PROG (K)
    ;; If the node has the grammar rule arc symbol as a property, and its value is an atom,
    ;; then set K to a list of just that.
    (COND ((ATOM (SETQ K (GET NODE (CAR GRUL)))) (SETQ K (NCONS K)))) 
    (PUTPROP NODE (CADR GRUL) 'LAB) ;; Change node label to the transition destination label
    ;; Now what does the arc symbol do?
        (RETURN
          (COND
            ;; Arc is SF = Simple function.  Only traverse if the relation is in the syntax net
            ;; Don't add anything to the output, and keep generating here if possible
            ((AND (GET (CAR GRUL) 'SF) (NULL (APPLY (CAR GRUL) (NCONS NODE)))) (FINDPATHS NODE))
            ;; Arc is TE = Terminal element.  value of the relation (K) is appended to the input, and generation
            ;; continues from the destination state.  E.g. VS, NS will finally emit the "verb string"
            ;; or "noun string" that has being built up through previous transitions.  And keep generating here.
            ((GET (CAR GRUL) 'TE) (APPEND K (FINDPATHS NODE)))
            ;; otherwise Arc is an EF = embedding function node, so we're going to start generating
            ;; at the destination node.  1. Set the label at the destination to be the arc symbol
            ;; 2. Recursively generate there.  When that's done, 3. keep generating here.
            ;; Wait, but it doesn't actually execute any function like the thesis says ...!  Oh well.
           (T (PUTPROP (CAR K) (CAR GRUL) 'LAB) (APPEND (FINDPATHS (CAR K)) (FINDPATHS NODE)))))))
            ;; note: no grammar symbols actually have the EF property,
            ;; it is assumed that if a node is not SF or TE (or DF) that it must be EF?

(DEFUN VOICE
  (NODE)
  (PROG (VBSTR SBJ TMP)
        (SETQ VBSTR (GET NODE 'LEX))
        (COND ((NOT (GET NODE 'VS_MADE)) (PUTPROP NODE VBSTR 'VS)))
        (COND ((SETQ TMP (GET NODE 'ACTSBJ)) (PUTPROP NODE (SETQ SBJ (CAR TMP)) 'SUBJ)))
        (PUTPROP NODE
                 (COND ((AND TMP (SETQ TMP (GET (CAR (GET SBJ 'LEX)) 'TYP))) TMP)
                       (T 'SING3))
                 'TYP)))


(DEFUN FORM
  (NODE)
  (COND
   ((NOT (GET NODE 'VS_MADE))
    (PROG (VBSTR)
          (COND
           ((EQUAL (GET NODE 'FORM) '(PROG))
            (SETQ VBSTR (GET NODE 'VS))
            (PUTPROP NODE (CONS 'BE (CONS (PRGRSIFY (CAR VBSTR)) (CDR VBSTR))) 'VS)))))))


(DEFUN PRGRSIFY
  (V)
  ((LAMBDA (TMP)
    (COND (TMP TMP)
          (T
           ((LAMBDA (X)
              (INTERN
               ;; JCM: made plenty of changes to code below for Common Lisp strings.
              (CONCATENATE 'STRING (COND ((EQ (CAR (LAST X)) #\E) (SUBSEQ X 0 (SUB1 (LENGTH X)))) (T (STRING V))) "ING")))
            (EXPLODE V)))))
   (GET V '!ING)))


(DEFUN MODAL
  (NODE) (PROG NIL (PUTPROP NODE (APPEND (GET NODE 'MODAL) (GET NODE 'VS)) 'VS)))


(DEFUN TENSE
  (NODE)
  (COND
   ((NOT (GET NODE 'VS_MADE))
    (PROG (VBSTR)
          (SETQ VBSTR (GET NODE 'VS))
          (SETQ !TYP (GET NODE 'TYP))
          (SETQ !NODE NODE)
          (PUTPROP NODE
                   (APPEND (APPLY (CAR (GET NODE 'TENSE)) (LIST (CAR VBSTR))) (CDR VBSTR))
                   'VS)))))


(DEFUN PRES
  (V)
  (NCONS
   (COND
    ((EQ !TYP 'SING3)
     ((LAMBDA (TMP) (COND (TMP TMP)
                          (T (INTERN (CONCATENATE 'STRING (STRING V) "S")))))
      (GET V 'SING3)))
    ((EQ V 'BE) (COND ((EQ !TYP 'SING1) 'AM) (T 'ARE)))
    (T V))))


(DEFUN PAST
  (V)
  (NCONS
   (COND
    ((AND (EQ V 'BE) (OR (EQ !TYP 'SING3) (EQ !TYP 'SING1))) 'WAS)
    (T
     ((LAMBDA (TMP)
       (COND (TMP TMP)
             ((EQ (CAR (LAST (EXPLODE V))) #\E) (INTERN (CONCATENATE 'STRING (STRING V) "D")))
             (T (INTERN (CONCATENATE 'STRING (STRING V) "ED")) )))
      (GET V 'PAST))))))



(DEFUN FUT
  (V) (CONS (CAR (PRES 'BE)) (LIST 'GOING 'TO (INFIN V))))


(DEFUN FUTPAST
  (V) (CONS (CAR (PAST 'BE)) (LIST 'GOING 'TO (INFIN V))))


(DEFUN PRESPAST
  (V) (PAST V))


(DEFUN PASTPAST
  (V) (PASTPERF V))


(DEFUN FUTFUT
  (V) (FUT V))


(DEFUN PRESFUT
  (V) (PRES V))


(DEFUN PASTFUT
  (V) (PAST V))


(DEFUN PASTPERF
  (V) (CONS 'HAD (NCONS (PERFECTIFY V))))


(DEFUN PERFECTIFY
  (V) ((LAMBDA (TMP) (COND (TMP TMP) (T (INTERN (CONCATENATE (STRING V) "ED"))))) (GET V '!EN)))


(DEFUN INFIN
  (V) ((LAMBDA (TMP) (COND (TMP TMP) (T (CAR (GET !NODE 'LEX))))) (GET V 'INF)))


(DEFUN MOOD
  (NODE) (PROG2 (PUTPROP NODE (CAR (GET NODE 'MOOD)) 'LAB) NIL))


(DEFUN CNDIT
  (NODE)
  (COND
   ((NOT (GET NODE 'VS_MADE))
    (PROG (VBSTR)
          (SETQ VBSTR (GET NODE 'VS))
          (SETQ !NODE NODE)
          (PUTPROP NODE (CONS 'WOULD (CONS (INFIN (CAR VBSTR)) (CDR VBSTR))) 'VS)))))


(DEFUN IVT
  (NODE)
  (PROG (VBSTR)
        (COND
         ((OR (CDR (SETQ VBSTR (GET NODE 'VS))) (EQUAL (GET NODE 'LEX) '(BE)))
          (PUTPROP NODE (PRELIST VBSTR 1) 'VS1)
          (PUTPROP NODE (CDR VBSTR) 'VS))
         (T (SETQ !NODE NODE)
            (PUTPROP NODE (INFIN (CAR VBSTR)) 'VS)
            (SETQ !TYP (COND ((GET NODE 'SUBJ) (GET NODE 'TYP)) (T 'SING3)))
            (PUTPROP NODE (APPLY (CAR (GET NODE 'TENSE)) (LIST 'DO)) 'VS1)))))


(DEFUN PRON
  (NODE)
  (PROG2 (PUTPROP NODE (LIST (GET (GET NODE 'PRON) (GET NODE 'CASE))) 'NS) NIL))


(DEFUN POSS
  (NODE) (PUTPROP (CAR (GET NODE 'POSS)) 'POSS 'CASE))


(DEFUN DET
  (NODE) (PROG2 (PUTPROP NODE (GET NODE 'DET) 'NS) NIL))


(DEFUN QUANT
  (NODE)
  (PROG (TMP)
        (SETQ TMP (CAR (GET NODE 'QUANT)))
        (PUTPROP NODE (APPEND (GET NODE 'NS) (NCONS TMP)) 'NS)
        (COND ((AND (NUMBERP TMP) (*GREAT TMP 1)) (PUTPROP NODE 'PL 'NBR)))))


(DEFUN NBR
  (NODE)
  (PROG (NOUN)
        (SETQ NOUN (CAR (GET NODE 'LEX)))
        (PUTPROP NODE ((LAMBDA (PRN) (COND (PRN PRN) (T 'IT))) (GET NOUN 'PRON)) 'PRON)
        (COND ((EQ (GET NODE 'NBR) 'PL) (SETQ NOUN (PLUR NOUN))))
        (COND ((EQ (GET NODE 'CASE) 'POSS) (SETQ NOUN (INTERN (CONCATENATE 'STRING (STRING NOUN) "S")))))
        (PUTPROP NODE (APPEND (GET NODE 'NS) (NCONS NOUN)) 'NS)))


(DEFUN PLUR
  (NOUN) (INTERN (CONCATENATE 'STRING (STRING NOUN) "S")))


(DEFUN DEG
  (NODE)
  (PROG (DEGREE ADJ)
        (SETQ ADJ (CAR (GET NODE 'LEX)))
        (COND ((SETQ DEGREE (GET NODE 'DEG)) (SETQ DEGREE (CAR DEGREE))) (T (SETQ DEGREE 'POS)))
        (PUTPROP NODE
                 (LIST (COND ((EQ DEGREE 'POS) ADJ) ((EQ DEGREE 'REL) (RELATIVEIZE ADJ))))
                 'MS)))


(DEFUN RELATIVEIZE
  (ADJ) ((LAMBDA (TMP) (COND (TMP TMP) (T (INTERN (CONCATENATE 'STRING (STRING ADJ) "ER"))))) (GET ADJ 'REL)))


(DEFUN POBJ
  (NODE) (PUTPROP (CAR (GET NODE 'POBJ)) 'OBJ 'CASE))


(DEFUN OBJ
  (NODE) (PUTPROP (CAR (GET NODE 'OBJ)) 'OBJ 'CASE))


(DEFUN OBJ2
  (NODE) (PUTPROP (CAR (GET NODE 'OBJ2)) 'OBJ 'CASE))


(DEFUN SUBJ
  (NODE)
  (COND
   ((NOT (GET NODE 'DEL_SUBJ))
    (PROG (TMP)
          (COND
           ((NOT (AND (SETQ TMP (GET NODE 'INFOF)) (EQ (GET NODE 'SUBJ) (GET TMP 'SUBJ))))
            (RETURN
             (PUTPROP (GET NODE 'SUBJ)
                      (COND ((GET NODE 'OBJSUBJ) 'OBJ) (T 'NOM))
                      'CASE))))))))


(DEFUN NGT
  (NODE)
  (PROG (VBSTR)
        (COND
          ((NOT (OR (CDR (SETQ VBSTR (GET NODE 'VS))) (EQUAL (GET NODE 'LEX) '(BE))))
          (SETQ !NODE NODE)
          (SETQ !TYP (COND ((GET NODE 'SUBJ) (GET NODE 'TYP)) (T 'SING3)))
          (SETQ VBSTR (APPEND (APPLY (CAR (GET NODE 'TENSE)) (LIST 'DO)) (INFIN (CAR VBSTR))))))
    (PUTPROP NODE (CONS (CAR VBSTR) (APPEND (GET NODE 'NGT) (SUFLIST VBSTR 1))) 'VS)))


(DEFUN INF
  (NODE)
  (PROG (VNODE)
        (SETQ VNODE (CAR (GET NODE 'INF)))
        (PUTPROP VNODE (CONS 'TO (GET VNODE 'LEX)) 'VS)
        (PUTPROP VNODE T 'VS_MADE)
        (PUTPROP VNODE NODE 'INFOF)
        (PUTPROP VNODE T 'OBJSUBJ)
        (RETURN T)))


(DEFUN INF2
  (NODE)
  (PROG (VNODE)
        (SETQ VNODE (CAR (GET NODE 'INF2)))
        (PUTPROP VNODE (CONS 'TO (GET VNODE 'LEX)) 'VS)
        (PUTPROP VNODE T 'DEL_SUBJ)
        (PUTPROP VNODE T 'VS_MADE)
        (RETURN T)))


(DEFUN PRSNT
  (NODE)
  (PROG (VNODE)
        (SETQ VNODE (CAR (GET NODE 'PRSNT)))
        (PUTPROP VNODE (GET VNODE 'LEX) 'VS)
        (PUTPROP VNODE T 'VS_MADE)
        (PUTPROP VNODE T 'OBJSUBJ)
        (RETURN T)))


(DEFUN INST2
  (NODE)
  (PROG (VNODE)
        (SETQ VNODE (CAR (GET NODE 'INST2)))
        (PUTPROP VNODE (CONS 'BY (NCONS (PRGRSIFY (CAR (GET VNODE 'LEX))))) 'VS)
        (PUTPROP VNODE T 'DEL_SUBJ)
        (PUTPROP VNODE T 'VS_MADE)
        (RETURN T)))


(DEFUN SPRG
  (NODE)
  (PROG (VNODE)
        (SETQ VNODE (CAR (GET NODE 'SPRG)))
        (PUTPROP VNODE (CONS 'FROM (NCONS (PRGRSIFY (CAR (GET VNODE 'LEX))))) 'VS)
        (PUTPROP VNODE T 'VS_MADE)
        (PUTPROP VNODE T 'OBJSUBJ)
        (RETURN T)))


(DEFUN GSBJ
  (NODE)
  (PROG (VNODE)
        (SETQ VNODE (CAR (GET NODE 'GSBJ)))
        (PUTPROP VNODE (NCONS (PRGRSIFY (CAR (GET VNODE 'LEX)))) 'VS)
        (PUTPROP VNODE T 'VS_MADE)
        (PUTPROP VNODE T 'DEL_SUBJ)
    (RETURN T)))

;; JCM new function, is basically a copy of GSBJ
;; except for OBJSUBJ instead of DEL_SUBJ
(DEFUN GOBJ
  (NODE)
  (PROG (VNODE)
        (SETQ VNODE (CAR (GET NODE 'GOBJ)))
        (PUTPROP VNODE (NCONS (PRGRSIFY (CAR (GET VNODE 'LEX)))) 'VS)
        (PUTPROP VNODE T 'VS_MADE)
        (PUTPROP VNODE T 'OBJSUBJ) 
    (RETURN T)))



(DEFUN INF3
  (NODE)
  (PROG NIL
        (PUTPROP NODE (GET (CAR (GET NODE 'INF3)) 'ACTSBJ) 'ACTSBJ)
        (PUTPROP NODE (GET NODE 'INF3) 'INF2)
        (REMPROP NODE 'INF3)))


;; JCM 12/2020 added this for embedding in noun phrases
(DEFUN EMBED (NODE)
  (DOLIST (TARGET_GRAMMAR_SYM (REORDER_SEQUENCE '(ACTSBJ* SUBJ*)))
    ;; Randomize, try and bail out as soon as we find a working embedding
  (WHEN (TRY_EMBED_GRAMMAR_SYM NODE TARGET_GRAMMAR_SYM)
    (RETURN))))


;; Randomize the sequence of nodes to try, try, and then bail out
;; as soon as an embedding works.
(DEFUN TRY_EMBED_GRAMMAR_SYM (NODE SYM)
  (DOLIST (TARGET_NODE (REORDER_SEQUENCE (GETPROP NODE SYM)))
    (WHEN (NULL (GETPROP TARGET_NODE 'LAB)) ;; node hasn't been generated
      (PUTPROP TARGET_NODE 'S 'LAB)        ;; generate!
      ;; need to remove the link from the
      ;; parent node to  avoid infinite loops
      (REMPROP TARGET_NODE (REMOVE_LAST SYM))
      (RETURN (PUTPROP NODE (APPEND '(*COMMA* WHO) (FINDPATHS TARGET_NODE) '(*COMMA*)) 'ES)))))


(DEFUN REORDER_SEQUENCE (SEQ)
  (LET ((LEN (LIST-LENGTH SEQ)) (CUT))
    (WHEN (> LEN 0)
      (SETQ CUT (RANDOM LEN))
      (APPEND (NTHCDR CUT SEQ) (NBUTLAST (COPY-LIST SEQ) (- LEN CUT) )))))

(DEFUN REMOVE_LAST (SYM)
   (INTERN (COERCE (NBUTLAST (COERCE (STRING SYM) 'LIST)) 'STRING)))


(DEFUN OPENS
  (FILE)
  (PROG NIL
        (EVAL (LIST 'INC (LIST 'INPUT '(SIM NMG) FILE) NIL))
        (PRINT 'LOADING)
        (PRINC TAB)
        (PRINC FILE)))

(DEFUN SURFEXP
  (X) (COND ((ATOM X) (PRINT (NCONS X))) (T (GEN (CAR X)))))


(DEFUN INIT_SURF ()
  (PROG NIL
    (SETQ !BREAKING NIL)
    ;; This puts the property "TE" on certain grammar symbols
        (PROG (&V &L1 X)
              (SETQ &L1 '(LEX PREP VS VS1 NS MS PART1 PART2 MAN ES))
         LOOP (COND ((NULL &L1) (RETURN &V)))
              (SETQ X (CAR &L1))
              (SETQ &L1 (CDR &L1))
              (SETQ &V (PUTPROP X T 'TE))
          (GO LOOP))
    ;; This puts the property "SF" on certain grammar symbols
        (PROG (&V &L1 X)
              (SETQ &L1
                     '(VOICE FORM TENSE MOOD INF INF2 INF3 POBJ
                            SUBJ PNOM OBJ POSS DEG PRON OBJ2 MODAL
                            GSBJ IVT CNDIT NBR INST2 SPRG GOBJ QUANT DET
                            NGT PRSNT EMBED))
         LOOP (COND ((NULL &L1) (RETURN &V)))
              (SETQ X (CAR &L1))
              (SETQ &L1 (CDR &L1))
              (SETQ &V (PUTPROP X T 'SF))
          (GO LOOP))
        ;; This puts the property "DF" on certain grammar symbols
        (PROG (&V &L1 X)
              (SETQ &L1 '(NBR IVT CNDIT DEG EMBED))
         LOOP (COND ((NULL &L1) (RETURN &V)))
              (SETQ X (CAR &L1))
              (SETQ &L1 (CDR &L1))
              (SETQ &V (PUTPROP X T 'DF))
          (GO LOOP))
    ;; This puts the property "NOM" on certain symbols
    ;; New with DOLIST
    (DOLIST (X '(HE SHE IT THEY)) (PUTPROP X X 'NOM))
    ;; Old PROG version
        ;; (PROG (&V &L1 X)
        ;;       (SETQ &L1 (QUOTE (HE SHE IT THEY)))
        ;;  LOOP (COND ((NULL &L1) (RETURN &V)))
        ;;       (SETQ X (CAR &L1))
        ;;       (SETQ &L1 (CDR &L1))
        ;;       (SETQ &V (PUTPROP X X 'NOM))
        ;;       (GO LOOP))
    (SETF (GET 'BE 'PL) 'ARE)
    ;; There used to be more code here to read in the grammar
    ;; and lexicon from separate files (AFSTN and LEX1).
    ;; This is now handled by code just below
        )
  )

(DEFUN DEFLIST
  (PAIR_LIS PROP)
  (PROG (&V &LST1 X)
        (SETQ &LST1 PAIR_LIS)
   LOOP (COND ((NULL &LST1) (RETURN &V)) (T NIL))
        (SETQ X (CAR &LST1))
        (SETQ &V (PUTPROP (CAR X) (CADR X) PROP))
        (SETQ &LST1 (CDR &LST1))
        (GO LOOP)))

(DEFUN PUTPROP (ID VAL PROP) (SETF (GET ID PROP) VAL))
(DEFUN NCONS (X) (CONS X NIL))
(DEFUN EXPLODE (X) (COERCE (STRING X) 'LIST))

;; Set the !GR to be AFSTN.  In the old code we would read this from a
;; separate file.
(SETQ !GR 'AFSTN)
;; The AFSTN grammar, originally from file AFSTN (not GR1 !!)
;; But with many modifications.
;; This code populates the property list with it
;; (note, format is different from that in GR1)
(mapcar #'(lambda (x) (PUTPROP (CAR x) (CDR x) !GR))
        '(
          (S	(FIRS SNT1) (INF3 V0) V0)
          (V0	(VOICE V1))
          (SNT1	(LEX SNT2))
          (SNT2   (SECS T))
          (V1 	(FORM V2))
          (V2   (MODAL V3) V3)
          (V3	(TENSE V4))
           (V4  	(MOOD T))
          (COND   (CNDIT INDIC))
          (INTERROG (IVT INDIC))
          (SUBJUNC INDIC)
           (INDIC  (VS1 SBJ) SBJ)
           ;; JCM: below I added a direct transition to PRED
           ;; To support generating a verb clause without subject
           (SBJ  (SUBJ PRED) (GSBJ PRED) PRED)
           (PRED (MAN PRED1) PRED1)
          (PRED1	(NGT VP0) VP0)
          (VP0	(VS VP1))
          (VP1	(PART1 VP2) VP2)
          (VP2	(OBJ2 VP3) VP3)
          (VP3	(PP1 VP4) VP4)
          (VP4	(P_ADJ VP5) VP5)
          (VP5	(LOC VP6) VP6)
          (VP6    (OBJ VP7) VP7)
          (VP7	(PART2 VP8) VP8)
           (VP8	(IOBJ VP9) VP9)
           ;; JCM added GOBJ to the below
          (VP9	(INF VP10) (INF2 VP10)(S2 VP10)(S3 VP10)(SPRG VP10)(PRSNT VP10) (GOBJ VP10) VP10)
          (VP10	(INST T)(INST2 T))
          (INST2  S)
          (SPRG   S)
          (INF	S)
          (INF2   S)
          (S2	S)
          (FIRS	S)
           (SECS	S)
          (GSBJ	S)
          (PRSNT S)
           (GOBJ S) ;; JCM new addition
          (PP1	PNP)
          (INST	PNP)
          (IOBJ	PNP)
          (LOC	PNP)
          (PNP	(PREP PNP1))
          (PNP1	(POBJ T))
          (SUBJ	NP)
          (OBJ	NP)
          (POBJ	NP)
          (OBJ2   NP)
          (NP	(PRON NP3)(POSS NP1)(DET NP1) NP1)
          (NP1	(QUANT NP2) NP2)
          (NP2    (NBR NP3))
          (NP3	(NS NP4))
           (NP4	(PP1 NP5) NP5) ;; JCM added PP1 e.g. "cause of death"
           (NP5 (INF2 T) T)
;;           (NP5 (INF2 NP6) NP6);; JCM new addition, e.g. "inability to breathe"
           ;; could the PP1 and INF2 ever both occur? Don't think so
;;           (NP6 (EMBED NP7)) ;; JCM added for embedding clauses
;;           (NP7 (ES T)) ;; Prints "Embed string", result from embedding
          (POSS	(PRON NP3) (DET NP1) NP1)
          (P_ADJ  MOD)
          (MOD	(DEG MOD1))
          (MOD1	 (MS T))
          )
        )

;; Code for handling the lexicon.
;; In the old code we would read this from a
;; separate file.  And the file was formatted a bit differently
;; This code populates the property list with it
;; This time by just setf-ing symbol-plist  (DEFLIST (READ) !TMP!)))
(mapcar #'(lambda (x) (DEFLIST (CADR X) (CAR X)) )
                  '(
                    (SING3
                     ((BE IS)(GO GOES)(HAVE HAS)(DO DOES)(CAN CAN))
                     )
                    (PAST
                     ((BE WERE)(BECOME BECAME)(BUY BOUGHT) (CAN COULD)(COME CAME) (DO DID)(DRINK DRANK)(EAT ATE) (GET GOT)(GIVE GAVE)(GO WENT)(GRAB GRABBED) (HEAR HEARD)(HAVE HAD)(HIT HIT)(KNOW KNEW)(MAKE MADE)(READ READ) (STAB STABBED)(SEE SAW)(SELL SOLD)(TAKE TOOK)(TELL TOLD)(THINK THOUGHT) (LOSE LOST))
                     )
                    (!ING
                     ((BE BEING) (HAVE HAVING) (GRAB GRABBING) (STAB STABBING) (DIE DYING))
                     )
                    (!EN
                     ((BE BEEN)(BUY BOUGHT)(COME COME)(CAN BEEN-ABLE-TO) (DO DONE)(DRINK DRUNK)(EAT EATEN) (GET GOTTEN)(GIVE GIVEN)(GO GONE) (HAVE HAD)(HIT HIT)(HEAR HEARD) (KNOW KNOWN)(MAKE MADE)(READ READ) (SEE SEEN)(SELL SOLD)(TAKE TAKEN)(TELL TOLD)(THINK THOUGHT))
                     )
                    (PRON
                     ((JOHN HE) (BILL HE) (MARY SHE) (FRED HE) (HAMLET HE) (LAERTES HE) (OTHELLO HE) (IAGO HE) (CASSIO HE) (DESDEMONA SHE) (FALSTAFF HE)(SOMEONE HE))
                     )
                    (CONJ
                     ((AND T) (BECAUSE T))
                     )
                    (OBJ
                     ((HE HIM) (SHE HER) (IT IT) (THEY THEM) (I ME) (YOU YOU) (WE US))
                     )
                    (POSS
                     ((HE HIS) (SHE HER) (IT ITS) (THEY THEIR) (I MY) (YOU YOUR) (WE OUR))
                     )
                    (INF
                     ((CAN BE_ABLE_TO) (IS BE)(WAS BE)(HAD HAVE)(HAS HAVE))
                     )
          ) ;; ends big quoted s-exp
        ) ;; ends mapcar


(INIT_SURF)
