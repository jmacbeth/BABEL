;; This file based on PRPH.LSP in the CD directory of Neil Goldman's files.
;; Comments from some original MLISP code (a files called PRPH.SUP, BABEL, and INBAB in Goldman's
;; CD directory)
;; replaced GENSYM with GENTEMP everywhere.

;; VARIABLES
  ;; REAL -- number of realizations desired for a conceptualization
  ;;     (=1 in inference mode)
  ;; REAL_SO_FAR -- number of realizations produced so far
  ;; MAXPRPHS -- default value for REAL when in paraphrase mode
  ;; !TMP! -- used in initialization for reading from files; needed
  ;;      because of way ERRSET is implemented on system
  ;; !LPROP -- list of dotted pairs: (syntax net node . conceptual structure)
  ;;       used to see if a node already exists for a given conc.structure;
  ;;       i.e., if (MARY1) is to be generated more than once in a given
  ;;       realization (such as in top-level and embedded S) only a single
  ;;       syntax net node will be created for it
  ;; !NETPRINT -- a flag; syntax net is printed only if !NETPRINT is on (=T)
  ;; !BASETIME -- a dotted pair (time . tense)
  ;;          initially time of utterance (T-0) CONS 'PRES;
  ;;          changes to effect tensing of embedded Ss relative to their
  ;;          embedding Ss
  ;; QUOTED_HEAD -- used in processing concexicon frameworks; set whenever a
  ;;        quoted unit (like "do" or "something") is to be entered
  ;;        in syntax network

(DEFPARAMETER REAL T)
(DEFPARAMETER REAL_SO_FAR T)
(DEFPARAMETER MAXPRPHS T)
(DEFPARAMETER !TMP! T)
(DEFPARAMETER !LPROP T)
(DEFPARAMETER !NETPRINT T)
(DEFPARAMETER !BASETIME T)
(DEFPARAMETER QUOTED_HEAD T)
;; JCM May, 2022: these were added to support more sophisticated
;; paraphrase generation
(DEFPARAMETER INSERTED_HEAD T)
(DEFPARAMETER FORCE_NEW_TOKEN T)

;; will use this to implement OPENI and INC
;; can only open one file at a time
(DEFPARAMETER *GLOBAL-FILE-DESCRIPTOR* T)

;; Unconditionally close the file handler for now
(DEFUN INC (CHANNEL ACTION)
  (CLOSE *GLOBAL-FILE-DESCRIPTOR*))


;; A Bunch of functions that were needed that are part of
;; Either Lisp 1.6 or MLISP
(DEFUN PUTPROP (ID VAL PROP) (SETF (GET ID PROP) VAL))
;; Lisp 1.6 properties are implemented such that
;; Calling Get on something that is not a symbol just returns
;; NIL.  In Common Lisp this is an error, so replacing
;; many calls to GET with my own GETPROP, which checks
(DEFUN GETPROP (ID PROP) (AND (SYMBOLP ID) (GET ID PROP)))
(DEFUN *GREAT (x y) (> x y))
(DEFUN *LESS (x y) (< x y))
(DEFUN ADD1 (x) (1+ x))
(DEFUN SUB1 (x) (1- x))
(DEFUN *PLUS (x y) (+ x y))
(DEFUN *TIMES (x y) (* x y))
(DEFUN LSH (x y) (ASH x y))
(DEFUN ERRSET (X Y) (COND ((NULL X) X) (T (LIST X))))
(DEFUN PRINTSTR (X) (PRINT X))
(DEFUN SUFLIST (LIST INDEX) (NTHCDR INDEX LIST))
(DEFUN NCONS (X) (CONS X NIL))
(DEFUN EXPLODE (X) (COERCE (STRING X) 'list))
(DEFUN MEMQ (x y) (MEMBER X Y))

;; EXPR LISTIFY(S);
;; % return a list of all atoms in S-exp S   %
;; IF NULL S THEN NIL ELSE IF ATOM S THEN <S>
;; ELSE ?*APPEND(LISTIFY(CAR(S)), LISTIFY(CDR(S)));

(DEFUN LISTIFY
 (S) (COND ((NULL S) NIL) ((ATOM S) (LIST S)) (T (*APPEND (LISTIFY (CAR S)) (LISTIFY (CDR S))))))

;; EXPR DEFLIST (PAIR_LIS,PROP);
;; % For each (A V) in PAIR_LIS, associate value V with atom A under
;;   property P
;; %
;; FOR NEW PAIR IN PAIR_LIS DO  PUTPROP(CAR PAIR, CADR PAIR, PROP);

(DEFUN DEFLIST
 (PAIR_LIS PROP)
  (COND (PAIR_LIS (PUTPROP (CAAR PAIR_LIS) (CADAR PAIR_LIS) PROP) (DEFLIST (CDR PAIR_LIS) PROP))))

;; EXPR PREDIN(FILE);
;; % read the predicates from file ALLPS into array ALLPS	%
;; BEGIN
;; 	NEW N1;
;; 	OPENI(FILE);	N1←READ();
;; 	EVAL <'ARRAY,'ALLPS,'T,<'QUOTE, 1 CONS N1>>;
;; 	FOR NEW I←1 TO N1 DO ALLPS(I)←READ() CONS NIL;
;; 	INC(NIL,T);
;; END;


;; HAD TO MODIFY THIS SINCE COMMON LISP USES ARRAYS WITH 0-BASED INDICES
;; MADE THE ARRAY ONE ELEMENT LARGER THAN NEEDED
(DEFUN PREDIN
    (FILE)
  (PROG (N1)
     (OPENI FILE)
     (SETQ N1 (READ *GLOBAL-FILE-DESCRIPTOR*))
     (SETQ ALLPS (MAKE-ARRAY (1+ N1)))
     (DO ((I 1 (1+ I)))
	 ((EQUAL I N1))
      (SETF (AREF ALLPS I) (CONS (READ *GLOBAL-FILE-DESCRIPTOR*) NIL)))
     (INC NIL T)))

;; EXPR TREEIN(X);
;; % read in a discrimination net from file X.TRE and initialize it   %
;; BEGIN
;; 	OPENI(X CONS 'TRE);
;; 	SET(X,READ());
;; 	DOPRP(X,EVAL(X), 1);
;; 	INC(NIL,T)
;; END;

(DEFUN TREEIN
  (X) (PROG NIL
        (OPENTREE (CONS X (QUOTE TRE)))
        (SET X (READ *GLOBAL-FILE-DESCRIPTOR* NIL))
        (DOPRP X (EVAL X) 1)
        (INC NIL T)))


;; EXPR SCALESIN(FILE);
;; %Read in the property scales (normally from file SCALES  %
;; BEGIN
;; 	OPENI(FILE);
;; 	WHILE ¬ATOM(ERRSET(!TMP!←READ(),T)) DO
;; 	BEGIN
;; 		PUTPROP(!TMP!,READ(),'POSDIR);
;; 		PUTPROP(!TMP!,READ(),'NEGDIR);
;; 		PUTPROP(!TMP!,READ(),'SCALE);
;; 	END;
;; 	INC(NIL,T);
;; END;


(DEFUN SCALESIN
    (FILE)
  (PROG NIL
     (OPENI FILE)
     (LOOP WHILE (NOT (ATOM (ERRSET (SETQ !TMP! (READ *GLOBAL-FILE-DESCRIPTOR* NIL)) T)))
	DO
	  (PUTPROP !TMP! (READ *GLOBAL-FILE-DESCRIPTOR*) (QUOTE POSDIR))
	  (PUTPROP !TMP! (READ *GLOBAL-FILE-DESCRIPTOR*) (QUOTE NEGDIR))
	  (PUTPROP !TMP! (READ *GLOBAL-FILE-DESCRIPTOR*) (QUOTE SCALE)))
     (INC NIL T)))

;; EXPR FRAMESIN(FILE);
;; %read in the concexicon	(normally from file CXCN)	%
;; BEGIN
;; 	NEW VB;
;; 	OPENI(FILE);
;; 	WHILE ¬ATOM(ERRSET(!TMP!←READ(),T)) DO
;; 	BEGIN
;; 		PUTPROP(VB←CAR !TMP!,!TMP![2],'LEX);
;; 		PUTPROP(!TMP![2],!TMP![2],'INF);
;; 		PUTPROP(VB,!TMP![3],'FRAME);
;; 		PUTPROP(VB,CDDDR !TMP!,'SPEC_ACT)
;; 	END;
;; 	INC(NIL,T);
;; END;

(DEFUN FRAMESIN
  (FILE)
  (PROG (VB)
     (OPENI FILE)
     (LOOP WHILE (NOT (ATOM (ERRSET (SETQ !TMP! (READ *GLOBAL-FILE-DESCRIPTOR* NIL)) T)))
	DO 
	  (PUTPROP (SETQ VB (CAR !TMP!)) (CADR !TMP!) (QUOTE LEX))
	  (PUTPROP (CADR !TMP!) (CADR !TMP!) (QUOTE INF))
	  (PUTPROP VB (CADDR !TMP!) 'EXPECTED_SYNTAX)
	  (PUTPROP VB (CADDDR !TMP!) (QUOTE FRAME))
	  (PUTPROP VB (CDDDDR !TMP!) (QUOTE SPEC_ACT)))
     (INC NIL T)))

;; EXPR DICTIN(FILE);
;; % read in `semantic markers'	(normally from file CPRPS) %
;; BEGIN
;; 	OPENI(FILE);
;; 	WHILE ¬ATOM(ERRSET(!TMP!←READ(),T)) DO
;; 	BEGIN
;;  		PUTPROP(CAR !TMP!,CADR !TMP!,'LEX);
;; 		PUTPROP(CAR !TMP!,CDDR !TMP!,'CPRPS);
;; 	END;
;; 	INC(NIL,T);
;; END;


(DEFUN DICTIN
    (FILE)
  (PROG NIL
     (OPENI FILE)
     (LOOP WHILE (NOT (ATOM (ERRSET (SETQ !TMP! (READ *GLOBAL-FILE-DESCRIPTOR* NIL)) T)))
	DO
	  (PUTPROP (CAR !TMP!) (CADR !TMP!) (QUOTE LEX))
	  (PUTPROP (CAR !TMP!) (CDDR !TMP!) (QUOTE CPRPS)))
     (INC NIL T)))

;; EXPR DFCSIN(FILE);
;; % read in `defining characteristics' of verb senses used in POT_HEAD
;;   predicates.  (normally on file DEFCHS)		%
;; BEGIN
;; 	OPENI(FILE);
;; 	WHILE ¬ATOM(ERRSET(!TMP!←READ(),T)) DO
;; 		PUTPROP(CAR !TMP!,PREDPOINTS(CDR !TMP!),'DEFCHS);
;; 	INC(NIL,T);
;; END;


(DEFUN DFCSIN
    (FILE)
  (PROG NIL
     (OPENI FILE)
     (LOOP WHILE (NOT (ATOM (ERRSET (SETQ !TMP! (READ *GLOBAL-FILE-DESCRIPTOR* NIL)) T)))
	DO
	  (PUTPROP (CAR !TMP!) (PREDPOINTS (CDR !TMP!)) (QUOTE DEFCHS)))
     (INC NIL T)))

;; EXPR DOPRP(NAME,TREE,NODE);
;; % replaces integral references to predicates by actual pointers to
;;   those predicates, and integral refs to tree nodes by actual pointers
;;   to those nodes
;; %
;; IF NULL TREE THEN NIL
;; ELSE IF EQ(CAAR TREE,'T) THEN 		%a `terminal' node%
;; % TREE = ((T (response-list) predicate-list) left-subtree right-subtree) %
;; 	RPLACD(CDAR TREE,PREDPOINTS(CDDAR TREE)) ALSO
;; 	RPLACA(CDR TREE,GTTREE(NAME,CADR TREE)) 
;; ELSE (IF NUMBERP CAAR TREE THEN RPLACA(TREE,PREDPOINTS (CAR TREE)))
;; %  a non-terminal, `predicate' node 
;; TREE=((predicate list) left-subtree right-subtree)%
;; ALSO GOCHK(NAME,TREE,NODE); %process subtrees of TREE below NODE%


(DEFUN DOPRP (NAME TREE NODE)
  (COND ((NULL TREE) NIL)
        ((EQ (CAAR TREE) (QUOTE T))
         (RPLACD (CDAR TREE) (PREDPOINTS (CDDAR TREE)))
         (RPLACA (CDR TREE) (GTTREE NAME (CADR TREE))))
         ((EQ (CAAR TREE) (QUOTE G))
         (ERR (PRINT (QUOTE "GO TO FOUL-UP"))))
        (T
         (COND ((NUMBERP (CAAR TREE))
                (RPLACA TREE (PREDPOINTS (CAR TREE)))))
         (GOCHK NAME TREE NODE))))

;; EXPR GOCHK(NAME,TREE,NODE);
;; %fixes up `go to' subtrees of TREE if it has any. Also processes regular
;; subtrees.
;; %
;; FOR NEW INDX←2 TO 3 DO
;; 	IF TREE[INDX] THEN IF 'G EQ CAAR(TREE[INDX])
;; %TREE[INDX] = ((G.<node-index>))                     %
;; 		THEN RPLACA(TREE↓(SUB1 INDX),GTTREE(NAME,CDAR TREE[INDX]))
;; 		ELSE DOPRP(NAME,TREE[INDX],2*(SUB1 NODE)+INDX);

(DEFUN GOCHK
    (NAME TREE NODE)
  (DO ((INDX 2 (1+ INDX)))
      ((> INDX 3))
    (COND
      ((CAR (SUFLIST TREE (SUB1 INDX)))
       (COND
         ((EQ (QUOTE G) (CAAR (CAR (SUFLIST TREE (SUB1 INDX)))))
          (RPLACA (SUFLIST TREE (SUB1 INDX))
                  (GTTREE NAME (CDAR (CAR (SUFLIST TREE (SUB1 INDX)))))))
         (T
          (DOPRP NAME
                 (CAR (SUFLIST TREE (SUB1 INDX)))
                 (*PLUS (*TIMES 2 (SUB1 NODE)) INDX))))))))


;; EXPR PREDPOINTS(L);
;; % L is a list of indices of the array ALLPS. Collect a list of the predicates
;; stored in these array elements.  %
;; FOR NEW X IN L COLLECT
;; 	<LAMBDA(Z);
;; 	   IF NULL Z THEN PRINTSTR "NULL PREDICATE =" ALSO PRINC X
;; % an error indication -- null predicates should not be referenced %
;; 	   ELSE Z;
;; 	(ALLPS(X))>;


(DEFUN PREDPOINTS
    (L)
  (MAPCAN (LAMBDA (X)
	    (LIST
	     ((LAMBDA (Z) (COND ((NULL Z) (PRINTSTR (QUOTE "NULL PREDICATE =")) (PRINC X)) (T Z)))
	      (AREF ALLPS X))))
	  L))


;; EXPR GTTREE(NAME,NODE);
;; % return the subtree of tree named NAME headed by node # NODE %
;; IF NULL NODE THEN NIL ELSE
;; BEGIN
;; 	NEW TREE,BLIST;
;; 	TREE←EVAL(NAME);
;; 	NODE←LSH(NODE,1);
;; 	WHILE NEQ(NODE←LSH(NODE,-1),1) DO BLIST←CONS(BOOLE(1,NODE,1),BLIST);
;; 	FOR NEW X IN BLIST DO TREE←IF ZEROP(X)THEN TREE[2] ELSE TREE[3];
;; 	RETURN TREE
;; END;


;; the big idea of this function is to transform the node number into a bit sequence, and then

(DEFUN GTTREE
    (NAME NODE)
  (COND ((NULL NODE) NIL)
        (T (PROG (TREE BLIST)
              (SETQ TREE (EVAL NAME))
              (SETQ NODE (LSH NODE 1))
	      (LOOP WHILE (NOT (EQ (SETQ NODE (LSH NODE -1)) 1))
		 DO
		  (SETQ BLIST (CONS (MOD NODE 2) BLIST)))
	      (DOLIST (X BLIST)
		(SETQ TREE (COND ((ZEROP X) (CADR TREE)) (T (CADDR TREE)))))
                 (RETURN TREE)))))

;; EXPR OPENI(FILE);
;; %Opens a disc file for input	%
;; BEGIN
;; 	INC(EVAL<'INPUT,'(CD NMG),FILE>,NIL);
;; 	PRINT 'LOADING; PRINC TAB; PRINC FILE;
;; END;

(DEFUN OPENI
    (FILE)
  (PROG NIL
     (DEFPARAMETER *GLOBAL-FILE-DESCRIPTOR*
       (OPEN (COND ((LISTP FILE) (CONCATENATE 'STRING (STRING (CAR FILE)) "." (STRING (CDR FILE))))
                   (T (STRING FILE)))
             :DIRECTION :INPUT))
     (PRINT (QUOTE LOADING))
     (PRINC #\TAB)
    (PRINC FILE)))


;; JCM: created this to deal with moving the tree files into a
;; separate folder.  need a more elegant solution!

(DEFUN OPENTREE
    (FILE)
  (PROG NIL
     (DEFPARAMETER *GLOBAL-FILE-DESCRIPTOR*
       (OPEN (COND ((LISTP FILE) (CONCATENATE 'STRING "Discrimination Nets/" (STRING (CAR FILE)) "." (STRING (CDR FILE))))
                   (T (STRING FILE)))
             :DIRECTION :INPUT))
     (PRINT (QUOTE LOADING))
     (PRINC #\TAB)
     (PRINC FILE)))

;; % the functions on this page are used to apply a discrimination tree to 
;;   a conceptual representation, handling the control within the disc. net
;;   and the evaluation of predicates at the nodes				%

;; EXPR FIELD(FIELD_SPEC, C);
;; % Evaluate the FIELD_SPECification with respect to Conceptualization C	%
;; IF ATOM FIELD_SPEC THEN
;; 	(IF EQ(FIELD_SPEC,'ALL) THEN C ELSE
;; 	 IF EQ(FIELD_SPEC,'MAIN) THEN CAR C  ELSE
;; 	 IF EQ(FIELD_SPEC,'MODS) THEN CDR C)
;; 		   ELSE
;; FOR NEW ROLE_NAME IN FIELD_SPEC DO 
;;   C←NEXT(ROLE_NAME,C) UNTIL NULL C;


(DEFUN FIELD
 (FIELD_SPEC C)
  (COND ((ATOM FIELD_SPEC)
         (COND ((EQ FIELD_SPEC (QUOTE ALL)) C)
               ((EQ FIELD_SPEC (QUOTE MAIN)) (CAR C))
               ((EQ FIELD_SPEC (QUOTE MODS)) (CDR C))))
        (T
	 (PROG (&V &L1 ROLE_NAME)
                 (SETQ &L1 FIELD_SPEC)
            LOOP (COND ((NULL &L1) (RETURN &V)))
                 (SETQ ROLE_NAME (CAR &L1))
                 (SETQ &L1 (CDR &L1))
                 (SETQ &V (SETQ C (NEXT ROLE_NAME C)))
                 (COND ((NULL C) (RETURN &V)))
              (GO LOOP)))
	;; (DOLIST (ROLE_NAME FIELD_SPEC)
	;;       (SETQ C (NEXT ROLE_NAME C))
        ;;       (COND ((NULL C) (RETURN)))))
	))

;; EXPR NEXT(ROLE_NAME,L);
;; % Auxiliary function for FIELD		%
;; BEGIN
;; NEW L2;
;; IF (L2←IF GET(ROLE_NAME,'MOD_LINK) THEN CDR L ELSE CAR L) THEN
;; DO NIL UNTIL ( (ROLE_NAME EQ CAR L2) OR MTCH(ROLE_NAME,CAR L2)
;; 				     OR NULL(L2←CDDR L2));
;; RETURN (IF L2 THEN CADR L2
;; 	     ELSE IF EQ(MAINLNKC(L),'K) THEN NEXT(ROLE_NAME,NEXT('CON,L))
;; 					 ELSE NIL)
;; END;


(DEFUN NEXT
 (ROLE_NAME L)
  (PROG (L2)
     (COND
       ((SETQ L2 (COND ((GETPROP ROLE_NAME (QUOTE MOD_LINK)) (CDR L)) (T (CAR L))))
	(LOOP WHILE (NOT (OR (EQ ROLE_NAME (CAR L2)) (MTCH ROLE_NAME (CAR L2)) (NULL (SETQ L2 (CDDR L2)))))
	   DO '())))
     (RETURN
       (COND (L2 (CADR L2)) ((EQ (MAINLNKC L) (QUOTE K)) (NEXT ROLE_NAME (NEXT (QUOTE CON) L))) (T NIL)))))

;; EXPR MTCH(X,Y);
;; MEMQ(Y,GET(X,'MATCHES));

(DEFUN MTCH
 (X Y) (MEMQ Y (GETPROP X (QUOTE MATCHES))))


;; EXPR APTREE(TREE,CON_REP);
;; % The value of TREE is a discrimination net. CON_REP is the c-diag
;;   Applies the discrimination net to CON_REP; finds ALL responses,
;;   even when not in paraphrase mode.
;; %
;; BEGIN
;; 	NEW CLST,CTREE,PRED,TMP,HEADS,TFLAG,FFLAG;
;; 	IF TREE EQ 'AND THEN
;; %Conjunctions are placed in a `canonical' form; the element with earliest
;;  time is made the first
;; %
;; 	CLST←LAMBDA(T1,T2);
;; 	     IF EVAL<'TIM_REL,NIL,NIL,NIL,<'BEFORE,T1,T2>>
;; 	     THEN NCONS CON_REP
;; 	     ELSE IF EVAL<'TIM_REL,NIL,NIL,NIL,<'AFTER,T1,T2>>
;; 		  THEN NCONS MAKSYM(CON_REP);
;; 	     (CAR FIELD('(CON TIME),CON_REP),CAR FIELD('(?∧ TIME),CON_REP));
;; 	IF NULL CLST THEN CLST←CON_REP CONS IF GET(TREE,'SYMM) 
;; 					    THEN NCONS MAKSYM(CON_REP);
;;     	FOR NEW CD IN CLST DO
;; 	BEGIN
;; 		TFLAG←GENSYM();   FFLAG←GENSYM();
;; % A predicate is marked with TFLAG if it evaluates TRUE, and with FFLAG
;;   if it evaluates NIL.  By using GENSYMs, (non-INTERNed, of course), we
;;   needn't go through predicates erasing flags before each generation
;; %
;; 		CTREE←EVAL(TREE); % CTREE is the disc-net %
;; 		WHILE CTREE DO
;; 			IF EQ(CAR(PRED←CAR(CTREE)),'T) THEN
;; 			      (IF (TMP←TVAL(CDR PRED,CD,TFLAG,FFLAG)) 
;; 		% are terminal predicates satisfied? %
;; 				THEN HEADS←HEADS @ 
;; 				       FOR NEW X IN TMP COLLECT <X CONS CD>)
;; 			ALSO CTREE← CTREE[2] %follow `go-to' pointer %
;; 			ELSE CTREE←CTREE[2+PVAL(PRED,CD,TFLAG,FFLAG)];
;; 		%follow either true or false path %
;; 	END;
;; 	RETURN HEADS
;; END;



(DEFUN APTREE
  ;; % APTREE = APPLYTREE?  The value of TREE is a discrimination net. CON_REP is the c-diag
  ;;   Applies the discrimination net to CON_REP; finds ALL responses,
  ;;   even when not in paraphrase mode.
  ;; %
  ;; main purpose of this function is to take the discrimination net TREE and
  ;; a CD structure, and enumerate the "matches" as HEADS, which are the lexical
  ;; entries that could be used as part of constructing a sentence 
    (TREE CON_REP)
  (PROG (CLST CTREE PRED TMP HEADS TFLAG FFLAG)
     (COND
       ;; if it's the AND tree ...
       ;; Conjunctions are placed in a `canonical' form; the element with earliest time is made the first
         ((EQ TREE (QUOTE AND))
           (SETQ CLST
                ((LAMBDA (T1 T2)
                   (COND
                     ;; tried to fix the calls below so that BEFORE and AFTER are called properly
                     ;; More elegant solution needed.
                   ((EVAL (LIST (QUOTE TIM_REL) `'(NIL NIL NIL (LIST (QUOTE BEFORE) '(,T1 ,T2))))) (NCONS CON_REP))
                   ((EVAL (LIST (QUOTE TIM_REL) `'(NIL NIL NIL (LIST (QUOTE AFTER) '(,T1 ,T2)))))
                    (NCONS (MAKSYM CON_REP)))))
                 (CAR (FIELD (QUOTE (CON TIME)) CON_REP))
                  (CAR (FIELD (QUOTE (∧ TIME)) CON_REP))))))
    ;; Wasn't AND.  make CLST the CON_REP and if TREE is marked SYMM, then also stick on ... MAKSYM?
     (COND
       ((NULL CLST) (SETQ CLST (CONS CON_REP (COND ((GETPROP TREE (QUOTE SYMM)) (NCONS (MAKSYM CON_REP))))))))
     (DOLIST (CD CLST)
       (PROG NIL
              ; A predicate is marked with TFLAG if it evaluates TRUE, and with FFLAG
              ; if it evaluates NIL.  By using GENSYMs, (non-INTERNed, of course), we
              ; needn't go through predicates erasing flags before each generation
          (SETQ TFLAG (GENTEMP))
          (SETQ FFLAG (GENTEMP))
               ; CTREE is the discrimination-net
          (SETQ CTREE (EVAL TREE))
	  (LOOP WHILE (NOT (NULL CTREE))
	     DO
               (COND
                 ;; set PRED to the predicates and see 
                 ;; if this is a terminal node in the tree
                 ((EQ (CAR (SETQ PRED (CAR CTREE))) (QUOTE T))
                  (COND
                                        ; this is a terminal node
                                        ; are terminal predicates satisfied?
                    ((SETQ TMP (TVAL (CDR PRED) CD TFLAG FFLAG))
                                        ; if so, append entries to HEADS,
                                        ; one for each lexical item in the list
                     (SETQ HEADS
                           (APPEND HEADS
				   (MAPCAN (LAMBDA (X)
					     (LIST (CONS X CD)))
					   ;; (LIST (LIST (CONS 'HEAD X) (CONS 'CD CD))) ; ABK attempt 1
					   ;; (ASSOC_LIST X CD) ; ABK attempt 2
					   TMP)))))
                                        ; But terminal nodes have `go-to' pointers
                                        ; follow `go-to' pointer (if it exists ... other
                                        ; CTREE will be assigned to NIL
                  (SETQ CTREE (CADR CTREE)))
                                        ; not a terminal node in the tree
                                        ; follow either true or false path
                 (T (SETQ CTREE
                          (CAR
                           (SUFLIST CTREE
                                    (SUB1
                                     (*PLUS 2 (PVAL PRED CD TFLAG FFLAG)))))))))))
     (RETURN HEADS)))



;; EXPR PVAL(PREDLIST,CD,TFLAG,FFLAG);
;; %  PREDLIST has the form (P1,P2, ... ,Pn)  
;;    return 1 if all Pi are satisfied, else 0   %
;; BEGIN
;; 	NEW FLG;   FLG←T;
;; 	FOR NEW P IN PREDLIST DO NIL
;; 	UNTIL ¬(FLG←SMPVAL(P,CD,TFLAG,FFLAG));
;; 	RETURN IF FLG THEN 1 ELSE 0
;; END;

;; %  PREDLIST has the form (P1,P2, ... ,Pn)  
;;    return 1 if all Pi are satisfied, else 0   %
(DEFUN PVAL
    (PREDLIST CD TFLAG FFLAG)
  (PROG (FLG)
     (SETQ FLG T)
     (DOLIST (P PREDLIST)
       (COND ((NOT (SETQ FLG (SMPVAL P CD TFLAG FFLAG))) (RETURN))))
        (RETURN (COND (FLG 1) (T 0)))))


;; EXPR TVAL(TNODE,CD,TFLAG,FFLAG);
;; % TNODE has the form < <list of conceptual items>, P1,P2, ... Pn> %
;; IF ZEROP(PVAL(CDR TNODE, CD,TFLAG,FFLAG)) THEN NIL ELSE CAR TNODE;

(DEFUN TVAL
 (TNODE CD TFLAG FFLAG) (COND ((ZEROP (PVAL (CDR TNODE) CD TFLAG FFLAG)) NIL) (T (CAR TNODE))))


;; EXPR SMPVAL(PRED_WITH_FLAG,CD,TFLAG,FFLAG);
;; % PRED has the form (< function, field (of CD), 2nd arg > . FLAG) 
;;   This is the basic predicate eval function. First it checks
;;   to see if the flag field has already been set, indicating previous
;;   evaluation of this predicate w.r.t. this Con.Dep. structure
;; %
;; IF (CDR PRED_WITH_FLAG) EQ FFLAG THEN NIL
;; ELSE IF (CDR PRED_WITH_FLAG) EQ TFLAG THEN T
;; ELSE LAMBDA(PRED);
;; 	IF EVAL (PRED[1] CONS CD CONS FIELD (PRED[2], CD) CONS CDDR(PRED))
;; 	THEN RPLACD(PRED_WITH_FLAG,TFLAG) ALSO T
;; 	ELSE RPLACD(PRED_WITH_FLAG,FFLAG) ALSO NIL;
;;      (CAR PRED_WITH_FLAG);

(DEFUN SMPVAL
    (PRED_WITH_FLAG CD TFLAG FFLAG)
  (COND ((EQ (CDR PRED_WITH_FLAG) FFLAG) NIL)
        ((EQ (CDR PRED_WITH_FLAG) TFLAG) T)
        ;;  PRED has the form (< function, field (of CD), 2nd arg > . FLAG) 
        ;;  This is the basic predicate eval function. First it checks
        ;;  to see if The flag field has already been set, indicating previous
        ;;  evaluation of this predicate w.r.t. this Con.Dep. structure
        (T
         ((LAMBDA (PRED)
           (COND
             ((FUNCALL (CAR PRED) (LIST CD (FIELD (CADR PRED) CD) (THIRD PRED))) 
              ; (EVAL (CONS (CAR PRED) (CONS CD (CONS (FIELD (CADR PRED) CD) (CDDR PRED)))))
             (RPLACD PRED_WITH_FLAG TFLAG)
             T)
            (T (RPLACD PRED_WITH_FLAG FFLAG) NIL)))
          (CAR PRED_WITH_FLAG)))))

;; EXPR MAKSYM(CON_REP);
;; % CON_REP is a symmetric conceptual relation (like dual-cause).
;;   MAKSYM constructs the other version of the relation	
;; %
;; <CON_REP[1,1],CON_REP[1,4],CON_REP[1,3],CON_REP[1,2]>
;;    CONS LAMBDA(FCS);
;; 	SUBST(  SWITCH(CON_REP[1,1],CON_REP[1,3],FCS),
;; 		FCS,
;; 		CDR(CON_REP) );
;; 	(FIELD ('(FOCUS),CON_REP));

(DEFUN MAKSYM
 (CON_REP)
  (CONS (LIST (CAAR CON_REP) (CAR (CDDDAR CON_REP)) (CADDAR CON_REP) (CADAR CON_REP))
        ((LAMBDA (FCS) (SUBST (SWITCH (CAAR CON_REP) (CADDAR CON_REP) FCS) FCS (CDR CON_REP)))
         (FIELD (QUOTE (FOCUS)) CON_REP) )))


;; EXPR SWITCH(A,B,S_EXP);
;; % substitutes A for B and B for A everywhere in S_EXP.  A, B are atomic	%
;; IF ATOM S_EXP THEN
;;        (IF S_EXP EQ A THEN B
;; 	ELSE IF S_EXP EQ B THEN A
;; 	ELSE S_EXP)
;; ELSE SWITCH(A,B,CAR S_EXP) CONS SWITCH(A,B,CDR S_EXP);


(DEFUN SWITCH
 (A B S_EXP)
  (COND ((ATOM S_EXP) (COND ((EQ S_EXP A) B) ((EQ S_EXP B) A) (T S_EXP)))
        (T (CONS (SWITCH A B (CAR S_EXP)) (SWITCH A B (CDR S_EXP))))))


;; Former FEXPRs.  These seem to be fixed.

	;; % the functions on this page are the various predicates used
	;;   in the disc. nets						%

	;; % the following FEXPRs have the format:
	;;   X[1] = a Conceptual structure
	;;   X[2] = contents of some field of X[1]
	;;   X[3],X[4]  vary with particular predicate
	;; %

;; FEXPR EQU(X); % X[3] is a conceptual item, like *AIR* %
;; 	IF NULL X[2] THEN NULL X[3] ELSE CAR(X[2])=X[3];

(DEFUN EQU
 (X) (COND ((NULL (CADR X)) (NULL (CADDR X))) (T (EQUAL (CAR (CADR X)) (CADDR X)))))

;; FEXPR ID(X); % X[3] is a field-specification %
;;       CAR(X[2]) = CAR FIELD(X[3],X[1]);

(DEFUN ID
 (X) (EQUAL (CAR (CADR X)) (CAR (FIELD (CADDR X) (CAR X)))))

;; FEXPR DIF(X); NULL EVAL(CONS('ID,X));

(DEFUN DIF
    ;; Avoiding evaluating args, need a better fix
    ;; (X) (NULL (EVAL (CONS (QUOTE ID) `',X))))
 (X) (NULL (ID X)))

(DEFUN POSM
 (X)
  (OR (NULL (CADR X)) (NOT (OR (MEMQ (QUOTE *NEG*) (CAR (CADR X))) (MEMQ (QUOTE *CANNOT*) (CAR (CADR X)))))))

;; FEXPR MMQ(X);  % X[3] is a conceptual token, like *NEG*  %
;; 	IF X[2] THEN MEMQ(X[3],CAR X[2]);

(DEFUN MMQ
 (X) (COND ((CADR X) (MEMQ (CADDR X) (CAR (CADR X))))))

;; FEXPR PROP(X);  % X[3] is a conceptual property, like FLUID %
;; 	IF NULL X[2] THEN NIL ELSE MEMQ(X[3],GET(CAR X[2], 'CPRPS));

(DEFUN PROP
 (X) (COND ((NULL (CADR X)) NIL) (T (SATF (CADDR X) (GETPROP (CAR (CADR X)) (QUOTE CPRPS))))))

;; FEXPR POT_HEAD(X);
;; 	% is X[3] a possible word-sense head for conceptualization X[2]? %
;; 	X[2] &
;; 	¬ZEROP PVAL(GET(X[3],'DEFCHS),X[2],GENSYM(),GENSYM());


(DEFUN POT_HEAD
 (X) (AND (CADR X) (NOT (ZEROP (PVAL (GETPROP (CADDR X) (QUOTE DEFCHS)) (CADR X) (GENTEMP) (GENTEMP))))))

;; FEXPR SKEL(X);
;; 	%  is X[3] the skeleton of conceptualization X[2] ?  %
;; 	EQ(X[3], CTYP(X[2]) );


(DEFUN SKEL
 (X) (EQ (CADDR X) (CTYP (CADR X))))

;; FEXPR MNLK(X);
;; 	%  is X[3] the main link of conceptualization X[2] ?  %
;; 	EQ(X[3], MAINLNK(X[2]) );

(DEFUN MNLK
 (X) (EQ (CADDR X) (MAINLNK (CADR X))))

;; FEXPR MNLKC(X);
;; 	%  is X[3] the code of the main link of conceptualization X[2] ?  %
;; 	EQ(X[3], MAINLNKC(X[2]) );


(DEFUN MNLKC
 (X) (EQ (CADDR X) (MAINLNKC (CADR X))))

;; FEXPR GRREAT(X);
;; 	% is CAR(X[2]) > X[3]	%
;; 	NUMBERP CAR(X[2]) & NUMBERP(X[3]) & (CAR(X[2]) ?*GREAT X[3

(DEFUN GRREAT
 (X) (AND (NUMBERP (CAR (CADR X))) (NUMBERP (CADDR X)) (*GREAT (CAR (CADR X)) (CADDR X))))

;; FEXPR LESSS(X);
;;      % is CAR(X[2]) < X[3]	%
;;      NUMBERP CAR(X[2]) & NUMBERP(X[3]) & (CAR(X[2]) ?*LESS X[3]

(DEFUN LESSS
 (X) (AND (NUMBERP (CAR (CADR X))) (NUMBERP (CADDR X)) (*LESS (CAR (CADR X)) (CADDR X))))

(DEFUN MEM_QUERY
 (X)
  ((LAMBDA (BIND_LIST) (COND (BIND_LIST (PROVE (FILLPAT BIND_LIST (CADDDR X))))))
   (GET_BINDINGS (CAR X) (CADDR X))))

;; FEXPR MEM_QUERY2(X);
  ;;       % X[3] is a list of field specifications.
  ;;         X[4] an arbitrary S-expression, in which the form (↑|$  n)
  ;;         is used to refer to the value of the nth field-spec
  ;;       %
  ;;       LAMBDA (BIND_LIST);
  ;;       IF BIND_LIST THEN
  ;;       	PROVE2(FILLPAT(BIND_LIST,X[4]));
  ;;       (GET_BINDINGS(X[1],X[3]));


(DEFUN MEM_QUERY2
    (X)
  ((LAMBDA (BIND_LIST) (COND (BIND_LIST (PROVE2 (FILLPAT BIND_LIST (CADDDR X))))))
   (GET_BINDINGS (CAR X) (CADDR X))))


;; FEXPR TIM_REL(X);
  ;;       % X[3] a list of 1 or 2 field specs which refer to TIME fields.
  ;;         X[4] =(BEFORE|AFTER  (↑ n1) (↑ n2)),  where ni is 1 or 2.
  ;;       %
  ;;       LAMBDA (BIND_LIST);
  ;;       IF BIND_LIST THEN
  ;;       	TIMPROVE(FILLPAT(BIND_LIST,X[4]));
  ;;       (GET_BINDINGS(X[1],X[3]));


(DEFUN TIM_REL
 (X)
  ((LAMBDA (BIND_LIST) (COND (BIND_LIST (TIMPROVE (FILLPAT BIND_LIST (CADDDR X))))))
   (GET_BINDINGS (CAR X) (CADDR X))))

;; From PRPH.SUP
;; FEXPR BEFORE(X); MAKNAM(CDR EXPLODE CAR(X)) LESSP MAKNAM(CDR EXPLODE CADR(X));


(DEFUN BEFORE
    ;; changed the MAKNAM construction to change things to integers here
    (X) (*LESS (PARSE-INTEGER (COERCE (CDR (EXPLODE (CAR X))) 'STRING))
               (PARSE-INTEGER (COERCE (CDR (EXPLODE (CADR X))) 'STRING))))

;; FEXPR AFTER(X); EVAL <'BEFORE, CADR X, CAR X>;

(DEFUN AFTER
    ;; Another fixed eval of an FEXPR below
 (X) (EVAL (LIST (QUOTE BEFORE) `'(,(CADR X) ,(CAR X)))))

;; end of FEXPRs

(DEFUN TIMPROVE
 (X) (EVAL X))

;; EXPR FILLPAT(BIND_LIST,PAT);
  ;;       % bind each variable -- indicated by ($ N) -- in PAT
  ;;         to the Nth BIND_LIST element	
  ;;         bind each variable -- indicated by (↑ N) -- in PAT
  ;;         to the car of the Nth BIND_LIST element	%
  ;;       IF ATOM PAT OR NULL BIND_LIST THEN PAT 
  ;;       ELSE IF CAR(PAT) EQ '?$ THEN BIND_LIST[CADR PAT]
  ;;       ELSE IF CAR(PAT) EQ '?↑ THEN CAR BIND_LIST[CADR PAT]
  ;;       ELSE FILLPAT(BIND_LIST,CAR PAT) CONS FILLPAT(BIND_LIST,CDR PAT);

(DEFUN FILLPAT
 (BIND_LIST PAT)
  (COND ((OR (ATOM PAT) (NULL BIND_LIST)) PAT)
        ((EQ (CAR PAT) (QUOTE $)) (CAR (SUFLIST BIND_LIST (SUB1 (CADR PAT)))))
        ((EQ (CAR PAT) (QUOTE ↑)) (CAR (CAR (SUFLIST BIND_LIST (SUB1 (CADR PAT))))))
        (T (CONS (FILLPAT BIND_LIST (CAR PAT)) (FILLPAT BIND_LIST (CDR PAT))))))


;; EXPR GET_BINDINGS(C_LEV,FIELD_LIST);
;; 	% make a list of the values of the field specs of FIELD_LIST applied
;; 	  to conceptual structure C_LEV
;; 	%
;; 	IF FIELD_LIST THEN
;; 	BEGIN
;; 		NEW TMP,RES;
;;  		RES←FOR NEW FIELD_SPEC IN FIELD_LIST COLLECT
;; 			(IF TMP←FIELD(FIELD_SPEC,C_LEV) THEN <TMP>)
;; 		    UNTIL NULL TMP;
;; 		IF TMP THEN RETURN RES
;; 	END
;; 	ELSE T;


(DEFUN GET_BINDINGS ; * ABK NOTE: We think this works but when tracing we only see 'NIL 'NIL as arguments.
 (C_LEV FIELD_LIST)
  (COND (FIELD_LIST
         (PROG (TMP RES)
               (SETQ RES
		     (MAPCAN (LAMBDA (FIELD_SPEC)
			       (COND ((SETQ TMP (FIELD FIELD_SPEC C_LEV)) (LIST TMP))))
			     FIELD_LIST))
            (COND (TMP (RETURN RES)))))
        (T T)))

;; EXPR MAIN_LINK(CPT); 
;; 	%returns pair (<main link of CPT> . <code of main link>)  %
;; 	BEGIN
;; 		NEW CODE;
;; 		CPT←CAR CPT;
;; 		DO NIL UNTIL (CODE←GET(CAR CPT,'LNKCODE)) OR 
;; 			     NULL(CPT←CDDR CPT);
;; 		RETURN IF CODE THEN CAR(CPT) CONS CODE ELSE '(NIL . NIL);
;; 	END;

(DEFUN MAIN_LINK
 (CPT)
  (PROG (CODE)
     (SETQ CPT (CAR CPT))
     ;; uses short circuiting.  If CAR CPT doesn't have a LNKCODE
     ;; the setq CPT to CDDR CPT.  Why CDDR?  Because the concept structures
     ;; start with one or more _pairs_ before the "main link".  This is
     ;; why we see concepts with ((CON ((... )) <=> ... )) etc.
     (LOOP WHILE (NOT (OR (SETQ CODE (GETPROP (CAR CPT) (QUOTE LNKCODE))) (NULL (SETQ CPT (CDDR CPT)))))
	DO '())
     (RETURN (COND (CODE (CONS (CAR CPT) CODE)) (T (QUOTE (NIL)))))))

;; EXPR MAINLNKC(CPT); 
;; 	IF NOT ATOM(CAR CPT) THEN CDR MAIN_LINK(CPT);

(DEFUN MAINLNKC
 (CPT) (COND ((NOT (ATOM (CAR CPT))) (CDR (MAIN_LINK CPT)))))

;; EXPR MAINLNK(CPT); 
;; 	IF NOT ATOM(CAR CPT) THEN CAR MAIN_LINK(CPT);

(DEFUN MAINLNK
 (CPT) (COND ((NOT (ATOM (CAR CPT))) (CAR (MAIN_LINK CPT)))))

;; EXPR CTYP(CD);
;; 	%  returns skeleton of conceptualization CD  %
;; 	BEGIN
;; 		NEW CONTP;
;; 		CONTP←MAINLNKC(CD);
;; 		RETURN
;; 		IF EQ(CONTP,'K) THEN
;; 		 READLIST(MAINLNKC(CD[1,2]) CONS CONTP
;; 					    CONS NCONS MAINLNKC(CD[1,4]))
;; 		ELSE CONTP
;; 	END;

(DEFUN CTYP
 (CD)
  (PROG (CONTP)
        (SETQ CONTP (MAINLNKC CD))
        (RETURN
         (COND ((EQ CONTP (QUOTE K))
                (INTERN (CONCATENATE 'STRING (STRING (MAINLNKC (CADAR CD)))
                                       (STRING CONTP)
                                       (STRING (MAINLNKC (CAR (CDDDAR CD)))))))
               (T CONTP)))))


;; EXPR TNAM(CD);
	;; % which disc nets are appropriate for finding the word sense head
	;;   of CD? %
	;; BEGIN
	;; 	NEW SKEL,TMP2;
	;; 	SKEL←IF GET(CAR CD,'SCALE) THEN 'L ELSE CTYP(CD);
	;; 	RETURN
	;; 	IF SKEL='E THEN 'EVT CONS GET(CAR FIELD('(?<?=?>),CD),'TREES)
	;; 	ELSE
	;; 	IF SKEL='S THEN (IF TMP2←GET(CAR FIELD('(?<?≡?>),CD),'TREES)
	;; 				THEN TMP2  ELSE '(STAT))
	;; 	ELSE LAMBDA(X); IF X THEN X 
	;; 			ELSE IF CADR EXPLODE(SKEL) EQ 'K 
	;; 			     THEN NCONS 'KAUS;
	;; 	     (GET(SKEL,'TREES))
	;; END;


;; TNAM = Tree Name.  Returns the D-net .TRE file corresponding to the
;; SKEL Skeleton
(DEFUN TNAM
 (CD)
(PROG (SKEL TMP2)
;; had to change the line below, because in Common Lisp, attempting to GET a property list
;; off of something that is not a symbol throws an error.
        (SETQ SKEL (COND ((GETPROP (CAR CD) 'SCALE) (QUOTE L)) (T (CTYP CD))))
        (RETURN
         (COND ((EQUAL SKEL 'E) (CONS (QUOTE EVT) (GETPROP (CAR (FIELD (QUOTE (<=>)) CD)) (QUOTE TREES))))
               ((EQUAL SKEL (QUOTE S))
                (COND ((SETQ TMP2 (GETPROP (CAR (FIELD (QUOTE (<≡>)) CD)) (QUOTE TREES))) TMP2)
                      (T (QUOTE (STAT)))))
               (T
                ((LAMBDA (X) (COND (X X) ((EQ (CADR (EXPLODE SKEL)) (QUOTE K)) (NCONS (QUOTE KAUS)))))
                 (GETPROP SKEL (QUOTE TREES))))))))



;; % these are the functions which perform the `and-or' search thru the
;;   paraphrase space.  DO_HEADS iterates thru the disjuncts (alternate
;;   verb choices);  DO_FRAMES processes the `first' frame for a given
;;   choice, stacking the rest.  POPIT pops it (the stack)			%

;; JCM: new functionality!  checks to see that the syntax node
;; that we will be generating from is the syntax node expected
;; by the HEAD.  Now we can have frameworks for nouns!!!
;; One trick is that lots of syntax nodes transition to S, so
;; need to check for matching them all.
(DEFUN CHECK_MATCHING_SYNTAX (SCASE HEAD)
  (LET ((EXP_SYNTAX (GETPROP HEAD 'EXPECTED_SYNTAX)))
    (OR (NULL EXP_SYNTAX)
      (AND (EQ EXP_SYNTAX 'S) (MEMBER SCASE '(INF2 S2 FIRS SPRG INST2 GSBJ PRSNT SECS INF GOBJ EMBED-WHO)))
      (EQ EXP_SYNTAX SCASE))))


;; % SOURCE_NODE is a node of the syntax net being created (Source NODE?).
;;   SOURCE_CASE is a syntax relation (like ACTSBJ) (Source CASE?)
;;   HEADLIST is the result from FINDHEADS
;;

;; %	STACK = < <Ai Bi Ci Di> >
;; 	Ai= list of case frames (frameworks) not yet expanded
;; 	Bi= conceptual rep   
;; 	Ci= syntax net node to which frames must attach  
;; 	Di= !BASETIME when entry was put on stack
;; %

;; JCM: removed some unnecessary progs from this version and switched it
;; to dolist

;; JCM & ABK: code separated out from DO_HEADS function
(DEFUN CREATE_SYNTAX_NODE (SOURCE_NODE SOURCE_CASE)
  (LET ((TARGET_NODE (GENTEMP)))
    (PUTPROP SOURCE_NODE (LIST TARGET_NODE) SOURCE_CASE)
    (PUTPROP TARGET_NODE T 'GSYM)
    (CREATE_BACK_POINTER SOURCE_NODE SOURCE_CASE TARGET_NODE) ;; create back pointers
    TARGET_NODE))

(DEFUN ASSIGN_LEX_HEAD ()
  )

;; ABK: new function for logging headlist
(DEFUN LOG_HEADLIST (HEADLIST)
  (SETQ N 1)
  (DOLIST (HC HEADLIST)
    (FORMAT T (CONCATENATE 'STRING (WRITE-TO-STRING N) ". WORD SENSE HEAD:"))
    (PRINC #\TAB)
    (PRINC (CDR (ASSOC 'HEAD HC)))
    (FORMAT T "~%   CD STRUCTURE:")
    (PRINC #\TAB)
    (PRINC (CDR (ASSOC 'CD HC)))
    (SETQ N (+ N 1))
    (FORMAT T "~%~%")))

;; EXPR DO_HEADS(SNODE,SCASE,HEADLIST,STACK);
;; % SNODE is a node of the syntax net being created.
;;   SCASE is a syntax relation (like ACTSBJ)
;;   HEADLIST is the result from FINDHEADS
;; %
;; IF NULL HEADLIST THEN !FAILURE!←T ALSO POPIT(STACK) ELSE
;; BEGIN
;; 	NEW C_LEVEL,LEX_HEAD,HEAD,TNODE,LPNT,TMP,BT;
;; 	LPNT←LAST !LPROP; BT←!BASETIME;
;; 	% LPNT can now be RPLACD'd to lengthen !LPROP, or,
;; 	  if RPLACD'd with NIL, to restore it
;; 	  BT can be used to restore !BASETIME
;; 	%
;; 	FOR NEW HC IN HEADLIST DO
;; 	BEGIN
;; 		HEAD←CAR HC;
;; 		IF NOT(TMP←TOKEN(CDR HC))  THEN
;; 		BEGIN
;; 			PUTPROP(SNODE,NCONS(TNODE←GENSYM()),SCASE);
;; 			PUTPROP(TNODE,T,'GSYM);
;; 			PUTPROP(TNODE,NCONS(LEX_HEAD←LEX_ENT(HEAD)),'LEX);
;; 			RPLACD(LPNT,NCONS(TNODE CONS CDR HC));
;; 			C_LEVEL←SUBST(0,0,CDR HC);
;; 			DOSPECIAL(GET(HEAD,'SPEC_ACT),TNODE,C_LEVEL);
;; 		% performs special actions specified in concexicon entry%
;; 			IF C_LEVEL THEN	PROCESS_MODS(TNODE,LEX_HEAD,C_LEVEL);
;; 		% applies language specific functions needed to handle
;; 		  modifications of the HEAD
;; 		%
;; 			DO_FRAMES(C_LEVEL,TNODE,GET(HEAD,'FRAME),STACK);
;; 			RPLACD(LPNT,NIL); !BASETIME←BT;
;; % resores LPNT and !BASETIME for next paraphrase	%
;; 		END ELSE PUTPROP(SNODE,TMP,SCASE) ALSO POPIT(STACK);
;; 	END UNTIL EQ(REAL,REAL_SO_FAR);
;; % stop when sufficient paraphrases have been produced %
;; END;


(DEFUN DO_HEADS (SOURCE_NODE SOURCE_CASE HEADLIST STACK)
   (COND
    ((NULL HEADLIST) (POPIT STACK))    
    (T (PROG (C_LEVEL LEXICAL_HEAD HEAD CD FRAMEWORK TARGET_NODE LPNT TMP)
          (SETQ LPNT (LAST !LPROP))
					; !LPROP was a list of dotted pairs: (syntax net node . conceptual structure)
	  ;; !LPROP is now a list of lists of (syntax net node, conceptual structure, framework)
	  ;; see (DEFUN TOKEN) below
	     ;; LPNT is last pair in this list
         (dolist (HC HEADLIST)
	   (SETQ HEAD (CAR HC)) ;; HEAD is the lexical pointer to a CXCN entry
	   (SETQ CD (CDR HC))
	   ;(SETQ HEAD (CDR (ASSOC 'HEAD HC)))
	   ;(SETQ CD (CDR (ASSOC 'CD HC)))
           (SETQ FRAMEWORK (GETPROP HEAD 'FRAME)) ;; need FRAMEWORK earlier now
           (WHEN (CHECK_MATCHING_SYNTAX SOURCE_CASE HEAD)
             (COND
               (
                 ;; don't create a new syntax net node unless necessary
                 ;; JCM: FORCE_NEW_TOKEN forces it to happen in certain circumstances
                 ;; Particularly when ALL is used as a field specifier
                ;(OR (NOT (SETQ TMP (TOKEN (CDR HC) FRAMEWORK))) FORCE_NEW_TOKEN)
		(OR (NOT (SETQ TMP (TOKEN CD FRAMEWORK))) FORCE_NEW_TOKEN)
                ;; create a new syntax net node, TARGET_NODE = Target NODE?
		(SETQ TARGET_NODE (CREATE_SYNTAX_NODE SOURCE_NODE SOURCE_CASE))
		(PUTPROP TARGET_NODE (LIST (SETQ LEX_HEAD (LEX_ENT HEAD))) 'LEX)
		;(RPLACD LPNT (NCONS (LIST TARGET_NODE (CDR HC) FRAMEWORK)))
                (RPLACD LPNT (NCONS (LIST TARGET_NODE CD FRAMEWORK)))
		    ; replace conceptual structure of LPNT with ((TARGET NODE (CDR HC) FRAMEWORK))
                                        ; Changed to make a copy of the CD structure just
                                        ; before doing special actions that may mangle it
                                        ; (SETQ C_LEVEL (SUBST 0 0 (CDR HC)))
                ;(SETQ C_LEVEL (COPY-TREE (SUBST 0 0 (CDR HC))))
	        (SETQ C_LEVEL (COPY-TREE (SUBST 0 0 CD)))
                 (DOSPECIAL (GETPROP HEAD (QUOTE SPEC_ACT)) TARGET_NODE C_LEVEL)
                 (COND (C_LEVEL (PROCESS_MODS TARGET_NODE LEX_HEAD C_LEVEL)))
                                        ; Generate from the framework for this HEAD/CXCN entry
                                        ; This will recursively call DO_HEADS
                 (DO_FRAMES C_LEVEL TARGET_NODE FRAMEWORK STACK)
                 (RPLACD LPNT NIL))
                                        ; if we already have a syntax net node for this
                                        ; don't call DO_FRAMES, just connect to that node, POP off the stack
               (T (PUTPROP SOURCE_NODE TMP SOURCE_CASE)
                 (CREATE_BACK_POINTER SOURCE_NODE SOURCE_CASE (CAR TMP)) ;; create back pointers
                 (POPIT STACK)))
             (COND ((EQ REAL REAL_SO_FAR) (RETURN NIL))))) ;; return if we have generated enough paraphrases
           ))))


;; JCM 12/2020: this function is used to put back pointers in the network
;; But it has trouble with back pointers for prepositions, because
;; for those the MAKPREP function creates a preposition node in between before we get to
;; DO_HEADS... need to have a solution to generate embeddings from things such as
;; locations.
(DEFUN CREATE_BACK_POINTER (SOURCE_NODE SOURCE_CASE TARGET_NODE)
  (LET ((BACK_POINTER_SYM
          (INTERN (CONCATENATE 'STRING (STRING SOURCE_CASE) "*")) ))
    (PUTPROP
      TARGET_NODE
      (APPEND (GETPROP TARGET_NODE BACK_POINTER_SYM) (LIST SOURCE_NODE))
      BACK_POINTER_SYM)))

;; EXPR DO_FRAMES(C_LEVEL,SNODE,FRAMEWORK,STACK);
;; % C_LEVEL is a conceptual structure
;;   SNODE is a syntax net node
;;   FRAMEWORK is a list of unprocessed FRAMEs from a concexicon entry
;; %
;; IF NULL FRAMEWORK THEN POPIT(STACK) ELSE
;; BEGIN
;; 	NEW TCASE,DIRECS,FIELDSPEC,REQS,TNODE;
;; 	QUOTED_HEAD ← NIL;
;; 	TNODE←SNODE;  TCASE←CAAR FRAMEWORK;	DIRECS←CDAR FRAMEWORK;
;; 	IF DIRECS & (ATOM (CAR DIRECS) ∨ GET(CAAR DIRECS,'FIELD)) THEN
;; 		  FIELDSPEC←CAR DIRECS ALSO REQS←CDR DIRECS
;; 	ELSE
;; 		  FIELDSPEC←GET(TCASE,'FRAM_STDS) ALSO REQS←DIRECS;
;; 	% TCASE is the syntax relation specified in the FRAME
;; 	  FIELDSPEC is the field-specification specified, or the default
;; 	  for TCASE if none was specified
;; 	  REQS is the special requirements field
;; 	%
;; 	FOR NEW REQ IN REQS DO 
;; 	% process next special requirement	%
;; 		LAMBDA(NEWNC);
;; 			IF NEWNC THEN
;; 			   PUTPROP(TNODE←CAR NEWNC,T,'GSYM) ALSO
;; 			   TCASE←CDR NEWNC ;
;; 		(APPLY(CAR REQ, <CDR REQ,TCASE,SNODE,C_LEVEL>));
;; 	IF QUOTED_HEAD  THEN
;; 	% one of special requirements specified a `literal' head for
;; 	  the relation TCASE
;; 	%
;; 		(IF GET(TCASE,'NSTRUC) THEN
;; 		 PUTPROP(TNODE,<TNODE←GENSYM()>,TCASE) ALSO
;; 		 PUTPROP(TNODE,QUOTED_HEAD,'LEX) ALSO
;; 		 PUTPROP(TNODE,T,'GSYM)
;; 		 ELSE PUTPROP(TNODE,QUOTED_HEAD,TCASE)
;; 		)
;; 		ALSO DO_FRAMES(C_LEVEL,SNODE,CDR FRAMEWORK,STACK)
;; 	% we can now process the next FRAME in FRAMEWORK %
;; 	ELSE
;; 	DO_HEADS(TNODE,TCASE,FINDHEADS(FIELD(FIELDSPEC,C_LEVEL)),
;; 		 <CDR FRAMEWORK,C_LEVEL,SNODE,!BASETIME> CONS STACK)
;; 	% find all possible `heads' for the substructure specified by
;; 	  FIELDSPEC and process them with DO_HEADS
;; 	  unprocessed FRAMEs of FRAMEWORK must be stacked
;; 	%
;; END;


;; C_LEVEL is a conceptual structure
;; SOURCE_NODE is a syntax net node
;; FRAMEWORK is a list of unprocessed FRAMEs from a concexicon entry

(DEFUN DO_FRAMES
  (C_LEVEL SOURCE_NODE FRAMEWORK STACK)
  (COND
    ((NULL FRAMEWORK) (POPIT STACK))
    (T (PROG (TCASE DIRECS FIELDSPEC REQS TARGET_NODE)
         (SETQ QUOTED_HEAD NIL) ;; QTHD assigns QUOTED_HEAD
         (SETQ INSERTED_HEAD NIL) ;; INSHD assigns INSERTED_HEAD
         (SETQ FORCE_NEW_TOKEN NIL) ;; NTK assigns FORCE_NEW_TOKEN
         (SETQ TARGET_NODE SOURCE_NODE)
         ;; Getting the frame item
         (SETQ TCASE (CAAR FRAMEWORK))
         (SETQ DIRECS (CDAR FRAMEWORK))
         (COND
           ((AND DIRECS (OR (ATOM (CAR DIRECS)) (GETPROP (CAAR DIRECS) (QUOTE FIELD))))
             (SETQ FIELDSPEC (CAR DIRECS))
             (SETQ REQS (CDR DIRECS)))
           (T (SETQ FIELDSPEC (GETPROP TCASE (QUOTE FRAM_STDS))) (SETQ REQS DIRECS)))
         ;; TCASE is the syntax relation specified in the FRAME
         ;; FIELDSPEC is the field-specification specified, or the default
         ;; for TCASE if none was specified
         ;; REQS is the special requirements field
	  (DOLIST (REQ REQS)
	    ((LAMBDA (NEWNC)
                (COND
                  (NEWNC (PUTPROP (SETQ TARGET_NODE (CAR NEWNC)) T (QUOTE GSYM))
                    (SETQ TCASE (CDR NEWNC)))))
               (APPLY (CAR REQ) (LIST (CDR REQ) TCASE SOURCE_NODE C_LEVEL))))
         ;; one of special requirements specified a `literal' head for
	 ;; the relation TCASE. Basically this code is equivalent to a call to
         ;; DO_HEADS, but it doesn't do the embedded call to FINDHEADS, since our quoted
         ;; head is the only one we want, and since our quoted head doesn't have
         ;; a concexicon entry.
         (COND
           (QUOTED_HEAD (COND ((GETPROP TCASE 'NSTRUC)
                                (PUTPROP TARGET_NODE (LIST (SETQ TARGET_NODE (GENTEMP))) TCASE)
                                (PUTPROP TARGET_NODE QUOTED_HEAD 'LEX)
                                (PUTPROP TARGET_NODE T 'GSYM))
                          (T (PUTPROP TARGET_NODE QUOTED_HEAD TCASE)))
             ;; we can now process the next FRAME in FRAMEWORK %
             (DO_FRAMES C_LEVEL SOURCE_NODE (CDR FRAMEWORK) STACK))
           ;; JCM: new functionality!  INSERTED_HEAD actually has a concexicon entry, so
           ;; this will be like a call to DO_HEADS but without the call to FINDHEADS, just
           ;; the structure needed to do our INSERTED_HEAD
           (INSERTED_HEAD
             (DO_HEADS
               TARGET_NODE
               TCASE
               `((,(FIRST INSERTED_HEAD) ,(FIRST (FIELD FIELDSPEC C_LEVEL))))
	       ;(ASSOC_LIST (FIRST INSERTED_HEAD (FIRST (FIELD FIELDSPEC C_LEVEL))))
               (CONS (LIST (CDR FRAMEWORK) C_LEVEL SOURCE_NODE !BASETIME) STACK)))

           (T (DO_HEADS
                TARGET_NODE
                TCASE
                (FINDHEADS (FIELD FIELDSPEC C_LEVEL))
                (CONS (LIST (CDR FRAMEWORK) C_LEVEL SOURCE_NODE !BASETIME) STACK))))))))

(DEFUN ASSOC_LIST (HEAD CD)
  (LIST (LIST (CONS 'HEAD HEAD) (CONS 'CD CD))))

;; EXPR POPIT(STACK);
;; % pop the stack; if it is empty, syntax net is complete, so perform
;;   surface generation (syntax net --> sentence)
;; %
;; IF NULL STACK THEN REAL_SO_FAR←ADD1 REAL_SO_FAR ALSO
;; 		LAMBDA(H);
;; 	           BEGIN
;; 			NEW SAV;
;; 			IF NULL !FAILURE! THEN
;; 			SAV←NETCOPY(NETPRINT(CAR H,NIL,NIL,!NETPRINT))
;; 	%SAV saves a copy of the syntax net (it gets destroyed by SURFEXP)%
;; 		        ALSO SURFEXP(H)   ALSO RESTORENET(SAV)
;; 	%RESTORENET restores it.%
;; 			ELSE TERPRI NIL
;; 			ALSO PRINTSTR "UH ... UH ... MUMBLE"
;; 			ALSO TERPRI NIL ALSO !FAILURE!←NIL;
;; 		   END;
;; 		(GET('TOP_NODE,'S))
;; ELSE !BASETIME←STACK[1,4]
;; ALSO DO_FRAMES(STACK[1,2],STACK[1,3],STACK[1,1],CDR STACK);


(DEFUN POPIT
 (STACK)
  (COND (
          (NULL STACK) ;; nothing left on the stack, so we'll generate a realization!
          (SETQ REAL_SO_FAR (ADD1 REAL_SO_FAR)) ;; add to our count of realizations
          ((LAMBDA (H)
             (PROG (SAV)
               ;; save the semantic/syntax net 
               (SETQ SAV (NETCOPY (NETPRINT (CAR H) NIL NIL !NETPRINT)))
               ;; generate.  Just need the semantic net.
               ;; SURFEXP defined in surf.lisp, and the grammar is there
               (SURFEXP H) 
               (RESTORENET SAV) ;; restore
               (SETQ !BASETIME (QUOTE (T-0 . PRES))))) ;; reset !BASETIME
            (GETPROP (QUOTE TOP_NODE) (QUOTE S)))) ;; this will end up being H
    ;; Default: if stack is not empty, set !BASETIME
    ;; call DO_FRAMES with the next item on the stack (CADR is the second item)  
    (T (SETQ !BASETIME (CAR (CDDDAR STACK)))
      (WHEN (NULL (CAR !BASETIME))
        (FORMAT T "CAR OF !BASETIME WAS NULL.  WHY?  STACK WAS THIS:")
        (PRINT STACK))
      (DO_FRAMES (CADAR STACK) (CADDAR STACK) (CAAR STACK) (CDR STACK)))))


;; EXPR TOKEN(X);
;; % checks !LPROP to see if a syntax net node for X already exists	%
;; BEGIN
;;      NEW FLG;
;;      FOR NEW PAIR IN CDR !LPROP DO NIL
;;       UNTIL (X=CDR(PAIR)) & (FLG←NCONS(CAR PAIR));
;;      RETURN FLG
;; END;
;; 
;; EXPR FINDHEADS(C_LEVEL);
;; % either apply the disc nets for C_LEVEL or
;;   get the English-name for the `head'
;; %
;; IF C_LEVEL THEN IF ATOM C_LEVEL THEN <C_LEVEL CONS C_LEVEL> ELSE
;; LAMBDA(TRE_LIS); IF TRE_LIS THEN 
;; 		 FOR NEW TRE IN TRE_LIS COLLECT
;; 		   IF TRE EQ 'SCALE THEN  <ANSCALE(C_LEVEL) CONS C_LEVEL>
;; 				    ELSE APTREE(TRE,C_LEVEL)
;; 		 ELSE <GETNAME(CAR C_LEVEL) CONS C_LEVEL>;
;; (TNAM(C_LEVEL));


;; JCM: new functionality now checks to see that both the CD structure
;; and the FRAMEWORK that it was generated under match to determine
;; whether to reuse a particular syntax node
(DEFUN TOKEN
 (CD FRAMEWORK)
  (PROG (FLG)
     (SETQ TUPLE_LIST (CDR !LPROP))
     (DOLIST (TUPLE TUPLE_LIST)
       (COND ((AND (EQUAL (LIST CD FRAMEWORK) (CDR TUPLE)) (SETQ FLG (NCONS (CAR TUPLE)))))))
     (RETURN FLG)))


;; either apply the disc nets for C_LEVEL or get the English-name for the `head'

(DEFUN FINDHEADS
 (C_LEVEL)
  (COND
   (C_LEVEL
    (COND ((ATOM C_LEVEL) (LIST (CONS C_LEVEL C_LEVEL)))
    ;; (COND ((ATOM C_LEVEL) (ASSOC_LIST C_LEVEL C_LEVEL)))
          (T
           ((LAMBDA (TRE_LIS)
             (COND (TRE_LIS
		    (MAPCAN (LAMBDA (TRE)
			      (COND ((EQ TRE (QUOTE SCALE)) (LIST (CONS (ANSCALE C_LEVEL) C_LEVEL)))
                                    (T (APTREE TRE C_LEVEL))))
			    TRE_LIS))
                   (T (LIST (CONS (GETNAME (CAR C_LEVEL)) C_LEVEL)))))
	    ;; (T (ASSOC_LIST (GETNAME (CAR C_LEVEL)) C_LEVEL)))
            (TNAM C_LEVEL)))))))


;; EXPR ANSCALE(C_LEVEL);
;; % C_LEVEL = ( <scale-name> {VAL (<number>)} {INC (<number>)}  )	
;;   ANSCALE figures out the proper word to use for a point or change on a scale %
;; BEGIN
;; 	NEW SCALNAM,INC,SCALPT;
;; 	SCALNAM← CAR C_LEVEL;
;; 	IF (SCALPT←FIELD('(VAL),C_LEVEL)) & NUMBERP CAR SCALPT THEN
;; 	RETURN RUN_SCALE(GET(SCALNAM,'SCALE),CAR SCALPT)
;; 	ELSE
;; 	IF (INC←FIELD('(INC),C_LEVEL)) & NUMBERP CAR INC THEN
;; 	RETURN GET(SCALNAM, IF MINUSP CAR INC THEN 'NEGDIR ELSE 'POSDIR)
;; 	ELSE RETURN GET(SCALNAM,'POSDIR)
;; END;


(DEFUN ANSCALE
 (C_LEVEL)
  (PROG (SCALNAM INC SCALPT)
        (SETQ SCALNAM (CAR C_LEVEL))
        (COND
         ((AND (SETQ SCALPT (FIELD (QUOTE (VAL)) C_LEVEL)) (NUMBERP (CAR SCALPT)))
          (RETURN (RUN_SCALE (GETPROP SCALNAM (QUOTE SCALE)) (CAR SCALPT))))
         ((AND (SETQ INC (FIELD (QUOTE (INC)) C_LEVEL)) (NUMBERP (CAR INC)))
          (RETURN (GETPROP SCALNAM (COND ((MINUSP (CAR INC)) (QUOTE NEGDIR)) (T (QUOTE POSDIR))))))
         (T (RETURN (GETPROP SCALNAM (QUOTE POSDIR)))))))


;; EXPR RUN_SCALE(SCALE,POINT);
;; % scale is one of the conceptual state-scales.
;;   Find the lexical entry associated with the interval containing POINT  %
;; BEGIN
;; 	NEW CUR_CHOICE;
;; 	CUR_CHOICE←CAR SCALE;
;; 	IF SCALE←CDR SCALE THEN 
;; 	DO NIL UNTIL ?*LESS(POINT,CAR SCALE) OR ( (CUR_CHOICE←CADR SCALE) &
;; 						  NULL(SCALE←CDDR SCALE));
;; 	RETURN CUR_CHOICE
;; END;


(DEFUN RUN_SCALE
 (SCALE POINT)
  (PROG (CUR_CHOICE)
        (SETQ CUR_CHOICE (CAR SCALE))
        (COND
         ((SETQ SCALE (CDR SCALE))
	  (LOOP WHILE (NOT
                       (OR (*LESS POINT (CAR SCALE))
                       (AND (SETQ CUR_CHOICE (CADR SCALE)) (NULL (SETQ SCALE (CDDR SCALE))))))
	     DO '())))
     (RETURN CUR_CHOICE)))


;; EXPR LEX_ENT(SENSE);
;; LAMBDA(LE); IF LE THEN LE ELSE SENSE;
;; (GET(SENSE,'LEX));

(DEFUN LEX_ENT
 (SENSE) ((LAMBDA (LE) (COND (LE LE) (T SENSE))) (GETPROP SENSE (QUOTE LEX))))

;; % the SPECIAL REQUIREMENTs functions:		%

;; EXPR MAKPREP(REQ,CASE,NODE,C_LEVEL); 
;; % to get prepositions in front of NPs in the syntax net		%
;; BEGIN
;; 	NEW NNODE;
;; 	PUTPROP(NODE,<NNODE←GENSYM()>, CASE);
;; 	PUTPROP(NNODE,T,'GSYM); PUTPROP(NNODE,REQ,'PREP);
;; 	RETURN(NNODE CONS 'POBJ)
;; END;


(DEFUN MAKPREP
 (REQ CASE NODE C_LEVEL)
  (PROG (NNODE)
        (PUTPROP NODE (LIST (SETQ NNODE (GENTEMP))) CASE)
        (PUTPROP NNODE T (QUOTE GSYM))
        (PUTPROP NNODE REQ (QUOTE PREP))
        (RETURN (CONS NNODE (QUOTE POBJ)))))


;; EXPR QTHD(HEAD,CASE,NODE,C_LEVEL);
;; % to `quote' a piece of the syntax net			%
;; BEGIN
;; QUOTED_HEAD← HEAD
;; END;

(DEFUN QTHD
  (HEAD CASE NODE C_LEVEL) (PROG NIL (SETQ QUOTED_HEAD HEAD)))

(DEFUN INSHD
  (HEAD CASE NODE C_LEVEL) (PROG NIL (SETQ INSERTED_HEAD HEAD)))

(DEFUN NTK
 (HEAD CASE NODE C_LEVEL) (PROG NIL (SETQ FORCE_NEW_TOKEN T)))


;; (DEFUN MAKE_ARTICLE
;;   (HEAD CASE NODE C_LEVEL) (PROG NIL (SETQ FORCE_NEW_TOKEN T)))

;; EXPR ADDINC(NIL1,CASE,NODE,C_LEVEL);
;; BEGIN
;; QUOTED_HEAD←<ANSCALE(FIELD('(?<?≡?>T),C_LEVEL) @
;; 	LAMBDA(INC);
;; 		IF INC THEN <'INC,INC>;
;; 	(FIELD('(INC),C_LEVEL))
;; 		)>;
;; END;


(DEFUN ADDINC
 (NIL1 CASE NODE C_LEVEL)
  (PROG NIL
        (SETQ QUOTED_HEAD
              (LIST
               (ANSCALE
                (APPEND (FIELD (QUOTE (<≡>T)) C_LEVEL)
                        ((LAMBDA (INC) (COND (INC (LIST (QUOTE INC) INC))))
                         (FIELD (QUOTE (INC)) C_LEVEL))))))))


;; EXPR DOSPECIAL(SLIST,TNODE,C_LEV);
;; WHILE SLIST DO
;; PROG2(APPLY(CAR SLIST,<CADR SLIST,C_LEV,TNODE>),
;;       SLIST←CDDR SLIST
;;      );

(DEFUN DOSPECIAL
 (SLIST TARGET_NODE C_LEV)
  (LOOP WHILE (NOT (NULL SLIST))
     DO (PROG2 (APPLY (CAR SLIST) (LIST (CADR SLIST) C_LEV TARGET_NODE))
	      (SETQ SLIST (CDDR SLIST)))))

;; %	the `Language Specific' functions		%

;; EXPR PROCESS_MODS(NODE,LEXHEAD,C_L);
;; %  runs through the `modifiers' of a conceptualization
;;    making appropriate changes to the syntax net
;; %
;; BEGIN
;; 	NEW MODLIST,CACL;
;; 	IF MODLIST←CDR C_L THEN CACL←CAR C_L ALSO
;; 	DO MODHANDLER(NODE,CACL,CAR MODLIST, CADR MODLIST) 
;; 	UNTIL NULL(MODLIST←CDDR MODLIST);
;; 	IF (GET(LEXHEAD,'INF) & ¬GET(LEXHEAD,'CONJ) )THEN
;; 	% LEXHEAD is a verb; special stuff must be done %
;; 			!BASETIME←TENSER(NODE,C_L)
;; 			ALSO PUTPROP(NODE,CHOOSE_FORM(NODE,C_L), 'FORM)
;; 			ALSO PUTPROP(NODE,'(ACT), 'VOICE)
;; 	                ALSO IF NOT GET(NODE,'MOOD) THEN
;; 	     			PUTPROP(NODE,CHOOSE_MOOD(C_L), 'MOOD)
;; END;

;  runs through the `modifiers' of a conceptualization
;   making appropriate changes to the syntax net
;

(DEFUN PROCESS_MODS
 (NODE LEXHEAD C_L)
  (PROG (MODLIST CACL NEWBASETIME)
    ;; call modhandler on all modifiers
        (COND
         ((SETQ MODLIST (CDR C_L))
          (SETQ CACL (CAR C_L))
          (PROG (&V)
           LOOP (SETQ &V (MODHANDLER NODE CACL (CAR MODLIST) (CADR MODLIST)))
            (COND ((NULL (SETQ MODLIST (CDDR MODLIST))) (RETURN &V)) (T (GO LOOP))))))
    ;; Deal with tense and put in FORM, MOOD using
    ;; CHOOSE_FORM, CHOOSE_MOOD, always active voice
    (COND
         ((AND (GETPROP LEXHEAD 'INF) (NOT (GETPROP LEXHEAD 'CONJ)))
           (SETQ NEWBASETIME (TENSER NODE C_L))
           (IF (CAR NEWBASETIME) (SETQ !BASETIME NEWBASETIME))
           (PUTPROP NODE (CHOOSE_FORM NODE C_L) (QUOTE FORM))
           (PUTPROP NODE (QUOTE (ACT)) (QUOTE VOICE))
           (COND ((NOT (GETPROP NODE (QUOTE MOOD)))
                   (PUTPROP NODE (CHOOSE_MOOD C_L) (QUOTE MOOD))))))))

;; JCM June 2023: here is the version of PROCESS_MODS with an attempt to replace the PROG and LOOP
;; there's some problem with it, but eventually it should replace the one above

(DEFUN PROCESS_MODS_NEW
 (NODE LEXHEAD C_L)
  (PROG (MODLIST CACL NEWBASETIME)
    ;; call modhandler on all modifiers
        (COND
         ((SETQ MODLIST (CDR C_L))
          (SETQ CACL (CAR C_L))
	  (LOOP WHILE (NOT (NULL (SETQ MODLIST (CDDR MODLIST))))
	     DO (MODHANDLER NODE CACL (CAR MODLIST) (CADR MODLIST)))))
    ;; Deal with tense and put in FORM, MOOD using
    ;; CHOOSE_FORM, CHOOSE_MOOD, always active voice
    (COND
         ((AND (GETPROP LEXHEAD 'INF) (NOT (GETPROP LEXHEAD 'CONJ)))
           (SETQ NEWBASETIME (TENSER NODE C_L))
           (IF (CAR NEWBASETIME) (SETQ !BASETIME NEWBASETIME))
           (PUTPROP NODE (CHOOSE_FORM NODE C_L) (QUOTE FORM))
           (PUTPROP NODE (QUOTE (ACT)) (QUOTE VOICE))
           (COND ((NOT (GETPROP NODE (QUOTE MOOD)))
                   (PUTPROP NODE (CHOOSE_MOOD C_L) (QUOTE MOOD))))))))

;; EXPR MODHANDLER(NODE,CON_GOV,MODTYP,VAL);
;; %MODTYP is one of the modifying conceptual relations, such as
;;  POSS, PART, MANNER, etc
;; %
;; LAMBDA(FN); IF FN THEN	APPLY(FN,<NODE,CON_GOV,VAL>); (GET(MODTYP,'PROC));

;; MODTYP is one of the modifying conceptual relations, such as POSS, PART, MANNER, etc
(DEFUN MODHANDLER
 (NODE CON_GOV MODTYP VAL)
  ((LAMBDA (FN) (COND (FN (APPLY FN (LIST NODE CON_GOV VAL)))))
   (GETPROP MODTYP (QUOTE PROC))))

;; EXPR GET_DET(NODE,CON_GOV,REF); 
;; % choose an English determiner corresponding to the
;;   conceptual REF for the governor CON_GOV 
;; %
;; PUTPROP(NODE,IF CAR REF EQ 'DEF THEN '(THE)
;; 		ELSE CHOOSE_INDEF(CON_GOV),'DET);

(DEFUN GET_DET
 (NODE CON_GOV REF)
  (PUTPROP NODE (COND ((EQ (CAR REF) (QUOTE DEF)) (QUOTE (THE))) (T (CHOOSE_INDEF CON_GOV))) (QUOTE DET)))

;; EXPR CHOOSE_INDEF(CON_GOV);  
;; % select an indefinite determiner (A or SOME)	%
;; IF EVAL <'PROP, NIL, <CON_GOV>, 'MASS> THEN '(SOME) ELSE '(A);

(DEFUN CHOOSE_INDEF
    (CON_GOV)
  ;; another fixed eval of an FEXPR
(COND ((EVAL (LIST (QUOTE PROP) `'(NIL ,(LIST CON_GOV) ,(QUOTE MASS)))) (QUOTE (SOME))) (T (QUOTE (A)))))

;; EXPR GET_NBR(NODE,CON_GOV,QUANT);
;; PUTPROP(NODE,QUANT,'QUANT);

(DEFUN GET_NBR
 (NODE CON_GOV QUANT) (PUTPROP NODE QUANT (QUOTE QUANT)))

;; EXPR GET_MODE(NODE,CON_GOV,ML);
;; % processes MODE modifications	%
;; FOR NEW X IN CAR ML DO
;; BEGIN
;; 	NEW TMP;
;; 	TMP←GET(X,'MODE); %TMP is a list of MODEs%
;; 	WHILE TMP DO
;; 	BEGIN
;; 		PUTPROP(NODE,TMP[2],CAAR TMP);  TMP←CDDR TMP
;; 	END;
;; END;

(DEFUN GET_MODE
 (NODE CON_GOV ML)
  (DOLIST (X (CAR ML))			
    (PROG (TMP)
       (SETQ TMP (GETPROP X (QUOTE MODE)))
       (LOOP WHILE (NOT (NULL TMP))
	  DO (PUTPROP NODE (CADR TMP) (CAAR TMP)) (SETQ TMP (CDDR TMP))))))

;; EXPR GET_PART(NODE,CON_GOV,PRT);
;; % process PART modification of a conceptual nominal %
;; BEGIN
;; 	NEW NEWNODE;
;; 	IF NULL(NEWNODE←TOKEN(PRT)) THEN
;; 	NEWNODE←<GENSYM()> ALSO PUTPROP(CAR NEWNODE,T,'GSYM)
;; 	 ALSO PUTPROP(CAR NEWNODE,<LEX_ENT(CAR PRT)>,'LEX)
;; 	 ALSO !LPROP NCONC NCONS(CAR NEWNODE CONS PRT);
;; 	PUTPROP(NODE,NEWNODE,'POSS);
;; END;


(DEFUN GET_PART
 (NODE CON_GOV PRT)
  (PROG (NEWNODE)
    (COND
      ;; Not sure how this is used, but now TOKEN also checks the
      ;; framework for a match.  Assuming framework is NIL here
         ((NULL (SETQ NEWNODE (TOKEN PRT NIL)))
          (SETQ NEWNODE (LIST (GENTEMP)))
          (PUTPROP (CAR NEWNODE) T (QUOTE GSYM))
          (PUTPROP (CAR NEWNODE) (LIST (LEX_ENT (CAR PRT))) (QUOTE LEX))
          (NCONC !LPROP (NCONS (CONS (CAR NEWNODE) PRT)))))
        (PUTPROP NODE NEWNODE (QUOTE POSS))))

;; EXPR GET_OWN(NODE,CON_GOV,PRT);  GET_PART(NODE,CON_GOV,PRT);

(DEFUN GET_OWN
 (NODE CON_GOV PRT) (GET_PART NODE CON_GOV PRT))

;; EXPR GET_POSS(NODE,CON_GOV,PRT);  GET_PART(NODE,CON_GOV,PRT);

(DEFUN GET_POSS
 (NODE CON_GOV PRT) (GET_PART NODE CON_GOV PRT))

;; EXPR GET_CERT(NODE,CON_GOV,VAL);
;; % process CERTAINTY modification of a conceptualization %
;; IF CAR VAL LESSP .96 THEN
;; PUTPROP(NODE,NCONS RUN_SCALE(GET('CERTAINTY,'SCALE),CAR VAL),'MAN);

(DEFUN GET_CERT
 (NODE CON_GOV VAL)
  (COND
   ((*LESS (CAR VAL) 0.96000000)
    (PUTPROP NODE (NCONS (RUN_SCALE (GETPROP (QUOTE CERTAINTY) (QUOTE SCALE)) (CAR VAL))) (QUOTE MAN)))))

;; EXPR TENSER(NODE,CONCEPT);
;; % attach a TENSE relation to the syntax NODE, whose value is
;;   appropriate for expressing CONCEPT (a conceptualization
;; %
;; IF MAINLNKC(CONCEPT) MEMQ '(K A D) THEN TENSER(NODE,FIELD('(CON), CONCEPT))
;; % TENSE of a causal is determined from the antecedent %
;; ELSE LAMBDA(TIM);
;; 	PROG2(
;; 		PUTPROP(NODE,NCONS(GET_TNS(TIM)),'TENSE),
;; 		(CAR TIM CONS !NEWTENSE)
;; 		% this value becomes the new !BASETIME %
;; 	     );
;;      (FIELD( '(TIME),CONCEPT));

(DEFUN TENSER
 (NODE CONCEPT)
  (COND ((MEMQ (MAINLNKC CONCEPT) (QUOTE (K A D))) (TENSER NODE (FIELD (QUOTE (CON)) CONCEPT)))
        (T
         ((LAMBDA (TIM) (PROG2 (PUTPROP NODE (NCONS (GET_TNS TIM)) (QUOTE TENSE)) (CONS (CAR TIM) !NEWTENSE)))
          (FIELD (QUOTE (TIME)) CONCEPT)))))


;; EXPR GET_TNS(TIM);
;; IF TIM THEN T_REL_BASE(CAR TIM)
;; ELSE PRINTSTR "MISSING TIME"  ALSO !NEWTENSE←'PRES;
;; % TIM should really never be NIL; if some bug caused the TIME to be left
;;   off of a conceptualization, BABEL probably won't even get to this point.
;;   If it does, though, PRESent tense is chosen by default
;; %

(DEFUN GET_TNS
 (TIM) (COND (TIM (T_REL_BASE (CAR TIM))) (T (SETQ !NEWTENSE (QUOTE PRES)))))

;; EXPR T_REL_BASE(T1);
;; % select a TENSE appropriate for expressing time T1 relative to the
;;   !BASETIME
;; %
;; NTIM(!NEWTENSE←
;; 	IF EVAL<'TIM_REL,NIL,NIL,NIL,<'BEFORE,T1,CAR !BASETIME>> THEN 'PAST
;; 	ELSE IF EVAL<'TIM_REL,NIL,NIL,NIL,<'AFTER,T1,CAR !BASETIME>> 
;; 	     THEN 'FUT
;; 	ELSE 'PRES,
;;      CDR !BASETIME);


(DEFUN T_REL_BASE
 (T1)
  (NTIM (SETQ !NEWTENSE
              (COND
               ((FUNCALL 'TIM_REL (LIST NIL NIL NIL (BEFORE `(,T1 ,(CAR !BASETIME)))))
                (QUOTE PAST))
               ((FUNCALL 'TIM_REL (LIST NIL NIL NIL (AFTER `(,T1 ,(CAR !BASETIME)))))
                (QUOTE FUT))
               (T (QUOTE PRES))))
        (CDR !BASETIME)))

;; EXPR NTIM(T1,T2);
;; % creates `compound' tenses, like FUTPAST, when needed %
;; IF T2 EQ 'PRES THEN T1 ELSE AT(T1 CAT T2);

(DEFUN NTIM
    ;; this had been AT and CAT... fixed ...
 (T1 T2) (COND ((NOT (MEMQ T2 (QUOTE (PAST FUT)))) T1) (T (INTERN (CONCATENATE 'STRING (STRING T1) (STRING T2))))))

;; EXPR CHOOSE_FORM(NODE,C_L);
;; % SIMple or PROGressive form?   %
;; IF (GET(NODE,'TENSE) = '(PRES)) & (CTYP(C_L) NEQ 'S) THEN '(PROG)
;; ELSE '(SIM);

(DEFUN CHOOSE_FORM
 (NODE C_L)
  (COND
   ((AND (EQUAL (GETPROP NODE (QUOTE TENSE)) (QUOTE (PRES))) (NOT (EQ (CTYP C_L) (QUOTE S)))) (QUOTE (PROG)))
   (T (QUOTE (SIM)))))


;; EXPR CHOOSE_MOOD(C_L);
;; % INTERROGative, CONDitional, or INDICative MOOD?	%
;; IF QUESTP(C_L) THEN '(INTERROG)
;; ELSE IF MEMQ('?<?≡C,CAR C_L) THEN '(COND)
;; ELSE '(INDIC);


(DEFUN CHOOSE_MOOD
 (C_L)
  (COND ((QUESTP C_L) (QUOTE (INTERROG))) ((MEMQ (QUOTE <≡C) (CAR C_L)) (QUOTE (COND))) (T (QUOTE (INDIC)))))

;; EXPR QUESTP(C_L);
;; %should C_L be realized in INTERROGative mood?	%
;; BEGIN
;; 	NEW TMP,FLG;
;; 	IF TMP←FIELD('(MODE),C_L) & MEMQ('?*???* , TMP) THEN RETURN T;
;; 	FOR NEW X IN <CAR C_L, CDR C_L> DO
;; 		WHILE X & NULL FLG DO PROG2(FLG←EQ(X[2,1],'?*???*),X←CDDR X)
;; 	UNTIL FLG;
;; 	RETURN FLG
;; END;

(DEFUN QUESTP
 (C_L)
  (PROG (TMP FLG)
     (COND ((SETQ TMP (AND (FIELD (QUOTE (MODE)) C_L) (MEMQ (QUOTE *?*) TMP))) (RETURN T)))
     (DOLIST (X (LIST (CAR C_L) (CDR C_L)))
       (LOOP WHILE (AND X (NULL FLG))
	  DO (PROG2 (SETQ FLG (EQ (CAADR X) (QUOTE *?*))) (SETQ X (CDDR X))))
       (COND (FLG (RETURN))))
     (RETURN FLG)))

;; %  the SPECIAL ACTIONs:				%

;; EXPR ADDITIONS(ADDS,C_LEV,NODE);
;; FOR NEW ADDD IN ADDS DO 
;; LAMBDA(Z);
;; %CAR ADDD = role name
;;  CADR ADDD= field spec for info to fill role %
;;       IF Z THEN NCONC(IF GET(CAR ADDD,'MOD_LINK) THEN C_LEV ELSE CAR C_LEV,
;; 		      <CAR ADDD,SUBST(0,0,Z)>
;; 		       );
;; (FIELD(CADR ADDD,C_LEV));


(DEFUN ADDITIONS
 (ADDS C_LEV NODE)
  (DOLIST (ADDD ADDS)
              ((LAMBDA (Z)
                (COND
                 (Z (NCONC (COND ((GETPROP (CAR ADDD) (QUOTE MOD_LINK)) C_LEV) (T (CAR C_LEV)))
                           (LIST (CAR ADDD) (SUBST 0 0 Z))))))
               (FIELD (CADR ADDD) C_LEV))))

;; EXPR DELETIONS(DELS,C_LEV,NODE);
;; FOR NEW DEL IN DELS DO 
;; 	LAMBDA(CPART,ROLE);
;; 	      IF GET(ROLE,'MOD_LINK) THEN REMOVE_ROLE(CDR CPART,ROLE)
;; 	      ELSE REMOVE_ROLE(CAR CPART,ROLE);
;; 	(FIELD(ALL_BUT_LAST(DEL),C_LEV),CAR LAST DEL);


(DEFUN DELETIONS
 (DELS C_LEV NODE)
  (DOLIST (DEL DELS)
    ((LAMBDA (CPART ROLE)
       (COND ((GETPROP ROLE (QUOTE MOD_LINK)) (REMOVE_ROLE (CDR CPART) ROLE))
             (T (REMOVE_ROLE (CAR CPART) ROLE))))
     (FIELD (ALL_BUT_LAST DEL) C_LEV)
     (CAR (LAST DEL)))))

;; EXPR ALL_BUT_LAST(L);
;; IF NULL CDR L THEN 'ALL ELSE
;; COLLECT <CAR L> UNTIL NULL CDR(L←CDR L);

(DEFUN ALL_BUT_LAST
 (L)
  (COND ((NULL (CDR L)) (QUOTE ALL))
        (T (LET (NEWLIST '())
	     (LOOP WHILE (NOT (NULL (CDR (SETQ L (CDR L)))))
		DO (SETQ NEWLIST (APPEND NEWLIST (LIST (CAR L)))))))))

;; EXPR REMOVE_ROLE(L,ROLE);
;; 	IF L THEN
;; 	IF CAR L EQ ROLE THEN RPLACA(L,NIL) ALSO RPLACA(CDR L,NIL)
;; 	ELSE REMOVE_ROLE(CDDR L,ROLE);
;; %	simulation of memory functions			%


(DEFUN REMOVE_ROLE
 (L ROLE)
  (COND (L (COND ((EQ (CAR L) ROLE) (RPLACA L NIL) (RPLACA (CDR L) NIL)) (T (REMOVE_ROLE (CDDR L) ROLE))))))

;; EXPR GETNAME(SENSE);
;; % should really be able to ask memory for ENGLISH-NAME of SENSE,
;;   which is always some conceptual nominal like DOG1
;; %
;; LEX_ENT(SENSE);

;; Comments from PRPH.SUP start here, above are comments from original file named BABEL (which may not match perfectly)

;; EXPR GETNAME(SENSE);
;; LEX_ENT(SENSE);

(DEFUN GETNAME
 (SENSE) (LEX_ENT SENSE))


;; EXPR SATF(PRP,PLIST);
;; % is property PRP implied by any property of PLIST? %
;; MEMQ(PRP,PLIST)  ∨
;; BEGIN
;; 	NEW TMP;
;; 	RETURN FOR NEW X IN PLIST DO TMP←TSCH(PRP,X) UNTIL TMP;
;; END;

(DEFUN SATF
    (PRP PLIST)
  (OR (MEMQ PRP PLIST)
      (PROG (TMP)
         (RETURN
	   (DOLIST (X PLIST)
	     (SETQ TMP (TSCH PRP X))
	     (COND (TMP (RETURN))))))))


;; EXPR TSCH(PRP1,PRP2);
;; % is property PRP1 implied by property PRP2? %
;; EQ(PRP1,PRP2)  ∨
;; LAMBDA(TMP);
;; EQ(TMP,GET(PRP2,'PTREE))  &
;;  ANC(PRP1,PRP2,EVAL TMP) ;
;; (GET(PRP1,'PTREE));


(DEFUN TSCH
 (PRP1 PRP2)
  (OR (EQ PRP1 PRP2)
      ((LAMBDA (TMP) (AND (EQ TMP (GETPROP PRP2 (QUOTE PTREE))) (ANC PRP1 PRP2 (EVAL TMP))))
       (GETPROP PRP1 (QUOTE PTREE)))))

;; EXPR ANC(S1,S2,TREE);
;; % is S1 an ancestor of S2 in TREE? %
;; IF TREE & NEQ(S2,CAR TREE) THEN 
;; IF EQ(S1,CAR TREE) THEN MEMQ(S2,LISTIFY CDR(TREE))
;; ELSE 
;; BEGIN
;; 	NEW TMP;
;; 	RETURN  FOR NEW SBTR IN CDR TREE DO TMP←ANC(S1,S2,SBTR) UNTIL TMP;
;; END;


(DEFUN ANC
 (S1 S2 TREE)
  (COND
    ((AND TREE (NOT (EQ S2 (CAR TREE))))
     (COND ((EQ S1 (CAR TREE)) (MEMQ S2 (LISTIFY (CDR TREE))))
           (T (PROG (TMP)
                 (RETURN
		   (DOLIST (SBTR (CDR TREE))
		     (SETQ TMP (ANC S1 S2 SBTR))
		     (COND (TMP (RETURN)))))))))))

;; EXPR PROVE22(C_D);
;; BEGIN TERPRI NIL; PRINTSTR "TIME TO PLAY GOD -- IS THIS TRUE?";
;;       TERPRI NIL; PRINC C_D; RETURN READ() 
;; END;


(DEFUN PROVE22
 (C_D)
  (PROG NIL
;;        (TERPRI NIL)
;;        (PRINTSTR (QUOTE "TIME TO PLAY GOD -- IS THIS TRUE?"))
;;        (TERPRI NIL)
;;        (PRINC C_D)
        (RETURN T)))
;;        (RETURN (READ))))

;; EXPR PROVE(C_D); PROVE22(C_D);

(DEFUN PROVE
 (C_D) (PROVE22 C_D))

;; EXPR PROVE2(C_D);  PROVE22(C_D);

(DEFUN PROVE2
 (C_D) (PROVE22 C_D))

;; EXPR TIMPROVE(X);% X=(BEFORE|AFTER  Ta   Tb		%
;; EVAL X;

(DEFUN TIMPROVE
 (X) (EVAL X))

;; % top level program control, generator operating alone%
;; EXPR STARTUP ();
;; WHILE T DO
;; BEGIN
;; 	NEW TMP,SOURCE,C_LEVEL;
;; 	PRINTSTR TERPRI ("WHAT SOURCE FILE?");
;; 	OPENI(READ() CONS 'EX);
;; 	SOURCE←READ();
;; 	INC(NIL,T);
;; 	BREAK("LISP READ-EVAL LOOP TYPE P TO EXIT" , NIL);
;;         REAL←MAXPRPHS;
;; 	PRINTSTR "INDEX OF C-DIAGRAM?";
;; 	WHILE TMP←READ() DO
;; 	BEGIN 
;; 	      IF NUMBERP TMP THEN PRINT(C_LEVEL←SOURCE[TMP]);
;; 	      EXPRESS(NCONS C_LEVEL);
;; 	      PRINTSTR (CR CAT LF CAT "INDEX OF C-DIAGRAM?");
;; 	END;
;; END;


;; asks for a .EX file and an index into the file, then calls EXPRESS on that structure
;; JCM: Just to get started, I'm going to ignore this and I'm going to just call EXPRESS directly.  See just below EXPRESS
(DEFUN STARTUP
    NIL
  (LOOP WHILE T
     DO
       (PROG (TMP SOURCE C_LEVEL)
          (PRINTSTR (TERPRI (QUOTE "WHAT SOURCE FILE?")))
          (OPENI (CONS (READ) (QUOTE EX)))
          (SETQ SOURCE (READ))
          (INC NIL T)
          (BREAK (QUOTE "LISP READ-EVAL LOOP TYPE P TO EXIT") NIL)
          (SETQ REAL MAXPRPHS)
          (PRINTSTR (QUOTE "INDEX OF C-DIAGRAM?"))
	  (LOOP WHILE (SETQ TMP (READ))
	     DO
               (COND
                 ((NUMBERP TMP)
                  (PRINT
                   (SETQ C_LEVEL (CAR (SUFLIST SOURCE (SUB1 TMP)))))))
               (EXPRESS (NCONS C_LEVEL))
               (PRINTSTR
                (CAT CR (CAT LF (QUOTE "INDEX OF C-DIAGRAM?"))))))))

;; EXPR EXPRESS(C_L_LIST);
;; FOR NEW C_LEVEL IN C_L_LIST DO
;; BEGIN
;; 	NEW TMP;
;; 	REAL_SO_FAR←0; TMP←GENSYM(); CSYM(N0001);
;; 	REMPROP('TOP_NODE,'S); GC(); !LPROP←NIL CONS NIL; !BASETIME←'(T?-0 . PRES);
;; 	DO_HEADS('TOP_NODE, 'S, FINDHEADS(C_LEVEL),NIL);
;; 	TERPRI NIL; EVAL<'CSYM,TMP>
;; END;


(DEFUN EXPRESS
    (C_L_LIST)
  (DOLIST (C_LEVEL C_L_LIST) ; for every item C_LEVEL in the input list C_L_LIST
    (PROG (TMP)
       (SETQ REAL_SO_FAR 0)
       (SETQ TMP (GENTEMP))
       ;; (CSYM N0001) don't care about initializing the counter
       (REMPROP (QUOTE TOP_NODE) (QUOTE S))
       ;; (GC) triggers garbage collection
       (SETQ !LPROP (CONS NIL NIL))
       (SETQ !BASETIME (QUOTE (T-0 . PRES)))
       (DO_HEADS (QUOTE TOP_NODE) (QUOTE S) (FINDHEADS C_LEVEL) NIL)
       (TERPRI NIL)
       ;; Code below meant to update the generated symbol counter
       ;; gentemp is deprecated ... need to fix all of this
       ;; (EVAL (LIST (QUOTE CSYM) TMP))
       )))


;; %syntax net printing %
;; EXPR NETPRINT(NEWNODE,YET2PRINT,PRINTED,PRINTING);
;; PROG2(
;; 	IF NOT MEMQ(NEWNODE,PRINTED) THEN
;; 	BEGIN
;; 		NEW PRPLIST,TMP;
;; 		PRINTED←NEWNODE CONS PRINTED;
;; 		PRPLIST←CDR NEWNODE;
;; 		IF PRINTING THEN
;; 		  TERPRI NIL ALSO TERPRI NIL ALSO
;; 		  PRINC NEWNODE ALSO  PRINC COLON;
;; 		WHILE PRPLIST DO
;; 		BEGIN
;; 			IF NOT(CAR(PRPLIST) MEMQ '(PNAME GSYM)) THEN
;; 			TMP←CADR PRPLIST ALSO
;; 			(IF PRINTING THEN
;; 			  PRINC TAB ALSO PRINC CAR PRPLIST ALSO PRINC TAB
;; 			  ALSO PRINC TMP ALSO TERPRI NIL)
;; 			ALSO IF ¬NUMBERP(CAR TMP) & GET(CAR TMP,'GSYM)
;; 			     THEN YET2PRINT←CAR TMP CONS YET2PRINT;
;; 			PRPLIST←CDDR PRPLIST
;; 		END;
;; 	END,
;; 	IF YET2PRINT THEN NETPRINT(CAR YET2PRINT,CDR YET2PRINT,PRINTED,PRINTING)
;; 		     ELSE PRINTED
;;      );


(DEFUN NETPRINT
    (NEWNODE YET2PRINT PRINTED PRINTING)
  (PROG2 (COND
           ((NOT (MEMQ NEWNODE PRINTED))
            (PROG (PRPLIST TMP)
               (SETQ PRINTED (CONS NEWNODE PRINTED))
               ;; had to fix the line below ... in Lisp 1.6 you can CDR a symbol
               ;; to get at its property list.
               (SETQ PRPLIST (SYMBOL-PLIST NEWNODE))
               (COND (PRINTING (TERPRI NIL) (TERPRI NIL) (PRINC NEWNODE) (PRINC #\:)))
	       (LOOP WHILE (NOT (NULL PRPLIST))
		  DO
                    (COND
                      ;; While it's not the printname, or GSYM
                      ;; GSYMs are added to YET2PRINT
                      ((NOT (MEMQ (CAR PRPLIST) (QUOTE (PNAME GSYM))))
                       (SETQ TMP (CADR PRPLIST))
                       (COND
                         (PRINTING (PRINC #\TAB)
                                   (PRINC (CAR PRPLIST))
                                   (PRINC #\TAB)
                                   (PRINC TMP)
                                   (TERPRI NIL)))
                       (COND
                         ((AND (NOT (NUMBERP (CAR TMP))) (GETPROP (CAR TMP) (QUOTE GSYM)))
                          (SETQ YET2PRINT (CONS (CAR TMP) YET2PRINT))))))
                    (SETQ PRPLIST (CDDR PRPLIST))))))
      (COND (YET2PRINT (NETPRINT (CAR YET2PRINT) (CDR YET2PRINT) PRINTED PRINTING)) (T PRINTED))))

;; EXPR NETCOPY(NODELIST);
;; FOR NEW X IN NODELIST COLLECT <X CONS CDR X>;

; save the current property list of each node in NODELIST
(DEFUN NETCOPY
    (NODELIST)
  (MAPCAN (LAMBDA (X)
	    (LIST (CONS X (SYMBOL-PLIST X))))
	  NODELIST))

;; EXPR RESTORENET(L);
;; FOR NEW PR IN L DO
;; 	RPLACD(CAR PR,CDR PR);

(DEFUN RESTORENET
 (L)
  (DOLIST (PR L)
    (SETF (SYMBOL-PLIST (CAR PR)) (CDR PR))))

;; Now defined in surf.lisp
;; EXPR SURFEXP(X); NIL;
;; (DEFUN SURFEXP (X) NIL)

;; EXPR NET(); !NETPRINT←T;

(DEFUN NET NIL (SETQ !NETPRINT T))

;; EXPR NONET(); !NETPRINT←NIL;

(DEFUN NONET
 NIL (SETQ !NETPRINT NIL))

;; EXPR MEM(); PUTPROP('PROVE2,GET('PROVE2,'SUBR2),'SUBR);

(DEFUN MEM
 NIL (PUTPROP (QUOTE PROVE2) (GETPROP (QUOTE PROVE2) (QUOTE SUBR2)) (QUOTE SUBR)))

;; EXPR NOMEM(); PUTPROP('PROVE2,GET('PROVE22,'SUBR),'SUBR);

(DEFUN NOMEM
 NIL (PUTPROP (QUOTE PROVE2) (GETPROP (QUOTE PROVE22) (QUOTE SUBR)) (QUOTE SUBR)))

;; From INBAB.LSP and INBAB
;; This function initializes the Conceptual Dependency --> Syntax net part of the system 

(DEFPARAMETER !TMP! T)

;; EXPR INIT_GEN();
;; % This function initializes the Conceptual Dependency --> Syntax net
;;   part of the system and then REMPROPs itself
;; %
;; BEGIN
;; 	SPECIAL !TMP!;
;; 	NOUUO(T); ?*RSET(T);  BAKGAG (15); % LISP 1.6 debugging features %

;; 	MAXPRPHS←10; NET();  % 10 paraphrases; print syntax nets %

;; 	PUTPROP('PROVE2,GET('PROVE2,'SUBR),'SUBR2);

;; 	FOR NEW X IN '(?<?=?> ?<?≡?> ?<?≡ ?<?≡?>T ?<?≡?>F ?<?≡H ?<?≡C 
;; 		       ?∧ ?<?≡?≡?> OBJECT MOBJECT TO FROM CON INST ACTOR
;; 		       FOCUS VAL POSS PART) DO
;; 		PUTPROP(X,T,'FIELD); %internal names for conceptual roles%

;; 	FOR NEW X IN
;; 	 '((?*POSS?*.GET_POSS) (VAL.NIL) (TIME.NIL) (MODE.GET_MODE) (TF.NIL)
;; 	   (TS.NIL) (PART.NIL) (MANNER.NIL) (QUANTITY.GET_NBR)
;; 	   (FOCUS.NIL) (INC.NIL) (REF.GET_DET) (PART.GET_PART)
;; 	   (?*OWN?*.GET_OWN)  (CERTAINTY.GET_CERT))
;; 	DO
;; 	BEGIN
;; 		PUTPROP(CAR X,T,'MOD_LINK);
;; 		IF CDR X THEN PUTPROP(CAR X,CDR X,'PROC);
;; 	END;  % associations between conceptual modification types and
;; 		LANGUAGE-SPECIFIC functions of BABEL
;; 	      %

;; %	Associate codes with conceptual relation links            	%
;; 	DEFLIST( 
;; 	'( (?<?=?>   E) (?<?≡?>   S) (?∧  A) (?<?≡   K) (?<?≡H   K)
;; 	   (?<?≡C   K) (?<?≡?>T   C) (?<?≡?>F   C) (?<?≡?≡?> D) ),
;; 	'LNKCODE);

;; % syntax relations whose values are other syntax net nodes have the
;;   property NSTRUC on their property lists	%
;;         FOR NEW X IN '(ACTSBJ  OBJ  OBJ2  POBJ PP1 LOC IOBJ INST 
;; 			GSBJ FIRS SECS POSS P_ADJ 
;; 			INF INF2 INF3 S2 INST2 SPRG PRSNT)
;; 	DO PUTPROP(X,T,'NSTRUC);

;; % syntax net correspondence to conceptual MODEs	%
;; 	DEFLIST(
;; 		'((?*NEG?* ((NGT)(NOT))) (?*CAN?* ((MODAL)(CAN)))
;; 		  (?*CANNOT?* ((NGT)(NOT)(MODAL)(CAN)))  ),
;; 		'MODE);

;; 	DEFLIST(
;; 		'( (?<?≡ (?<?≡C ?<?≡H)) (TIME (TF TS)) ),
;; 		'MATCHES);

;; 	DEFLIST(
;; 		'( (DK T) (AND T) ),
;; 		'SYMM);

;; % read in default syntax case - conceptual role associations	%
;; 	OPENI('FMSTDS); 
;; 	WHILE ¬ATOM(ERRSET(!TMP!←READ(),T)) DO
;; 		PUTPROP(CAR !TMP!, CDR !TMP!, 'FRAM_STDS);
;; 	INC(NIL,T);

;; %	Read in state scales 	%
;; 	SCALESIN('SCALES);

;; %	Read in predicates referenced at tree nodes	%
;; 	PREDIN('ALLPS);

;; %	Bring in discrimination nets	%
;; 	OPENI('(TREES . NAM)); !TMP!←READ();
;; 	%!TMP! is a list of all the disc net files %
;; 	FOR NEW X IN READ() DO PUTPROP(CAR X, CDR X, 'TREES);
;; 	% that takes care of the disc-net to conceptual structure
;;           associations on TREES.NAM			%
;; 	INC(NIL,T);
;; 	FOR NEW X IN !TMP! DO TREEIN(X); %and that initializes the nets %

;; %	Read in conceptual dictionary -- word sense-property associations %
;; 	DICTIN('CPRPS);
;; %	Read in CONCEXICON	%
;; 	FRAMESIN('CXCN);
;; %	Read in DEFining CHaracteristicS	%
;; 	DFCSIN('DEFCHS);
;; 	REMPROP('INIT_GEN,'EXPR);
;; END;


(DEFUN INIT_GEN
    NIL
   (PROG NIL
      
      ;; Unneeded LISP 1.6 debugging features (NOUUO T) (*RSET T) (BAKGAG 17)

      ;; all of these work
        (SETQ MAXPRPHS 16) ;; max paraphrases
        ;; (NET) ;; print syntax nets
        (NONET) ;; don't print syntax nets
        ;; (PUTPROP (QUOTE PROVE2) (GETPROP (QUOTE PROVE2) (QUOTE SUBR)) (QUOTE SUBR2))

        ;; adds a property called FIELD to each of these conceptual roles
        ;; works, check!
        (DOLIST (X (QUOTE (<=> <≡> <≡ <≡>T <≡>F <≡H <≡C ∧ <≡≡> OBJECT MOBJECT
                               TO FROM CON INST ACTOR FOCUS VAL POSS PART)))
	  (PUTPROP X T (QUOTE FIELD)))

        ;; associations between conceptual modification types and LANGUAGE-SPECIFIC functions of BABEL
        ;; this works, check!
        (DOLIST (X (QUOTE
                     ((*POSS* . GET_POSS) (VAL) (TIME) (MODE . GET_MODE) (TF) (TS)
                      (PART) (MANNER) (QUANTITY . GET_NBR) (FOCUS) (INC)
                      (REF . GET_DET) (PART . GET_PART) (*OWN* . GET_OWN)
                      (CERTAINTY . GET_CERT))))
	  (PUTPROP (CAR X) T (QUOTE MOD_LINK))
	  (COND ((CDR X) (PUTPROP (CAR X) (CDR X) (QUOTE PROC)))))

        ;; Associate codes with conceptual relation links
        
        (DEFLIST (QUOTE ((<=> E) (<≡> S) (∧ A) (<≡ K) (<≡H K) (<≡C K) (<≡>T C)
                         (<≡>F C) (<≡≡> D)))
            (QUOTE LNKCODE))
        
        ;; syntax relations whose values are other syntax net nodes have the
        ;; property NSTRUC on their property lists
        ;; seems ok, check!
        (DOLIST (X (QUOTE
                     (ACTSBJ OBJ OBJ2 POBJ PP1 LOC IOBJ INST GSBJ FIRS SECS
                             POSS P_ADJ INF INF2 INF3 S2 INST2 SPRG PRSNT EMBED-WHO)))
	  (PUTPROP X T (QUOTE NSTRUC)))

        ;; syntax net correspondence to conceptual MODEs
	;; These seem to work, check!
        (DEFLIST (QUOTE ((*NEG* ((NGT) (NOT))) (*CAN* ((MODAL) (CAN))) (*CANNOT* ((NGT) (NOT) (MODAL) (CAN)))))
                 (QUOTE MODE))
        (DEFLIST (QUOTE ((<≡ (<≡C <≡H)) (TIME (TF TS)))) (QUOTE MATCHES))
        (DEFLIST (QUOTE ((DK T) (AND T))) (QUOTE SYMM))


        ;; read in default syntax case - conceptual role associations
        ;; works, check!
        (OPENI (QUOTE FMSTDS))
        (LOOP WHILE (NOT (ATOM (ERRSET (SETQ !TMP! (READ *GLOBAL-FILE-DESCRIPTOR* NIL)) T)))
	   DO (PUTPROP (CAR !TMP!) (CDR !TMP!) (QUOTE FRAM_STDS)))
        (INC NIL T)
        
        ;; Read in state scales
        ;; works! check!
        (SCALESIN (QUOTE SCALES))

        ;; Read in predicates referenced at tree nodes
        ;; works! check!
        (PREDIN 'ALLPS)
        
        ;; Bring in discrimination nets
        ;; works! check!
        (OPENTREE (QUOTE (TREES . NAM)))
        (SETQ !TMP! (READ *GLOBAL-FILE-DESCRIPTOR*))
        (DOLIST (X (READ *GLOBAL-FILE-DESCRIPTOR*))
	  (PUTPROP (CAR X) (CDR X) (QUOTE TREES)))
        (INC NIL T)


        ;; initialize the nets
        ;; works! check!
        (DOLIST (X !TMP!)
	  (TREEIN X))

        ;; Read in conceptual dictionary -- word sense-property associations
        ;; (DICTIN (QUOTE CPRPS.BIG))
        ;; There is a larger CPRPS, but format is not compatible
        (DICTIN (QUOTE CPRPS))


        ;; Read in CONCEXICON
        ;; works, check!
        (FRAMESIN (QUOTE CXCN))
        
        ;; Read in DEFining CHaracteristicS
        ;; works, check
        (DFCSIN (QUOTE DEFCHS))

        ;; No need for INIT_GEN to  REMPROP itself
        ;; (REMPROP (QUOTE INIT_GEN) (QUOTE EXPR))
        ))



(INIT_GEN)
