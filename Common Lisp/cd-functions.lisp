;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                          
;;;;  CD Functions
;;;;
;;;;  Common Lisp implementation by:
;;;;
;;;;  Bill Andersen (waander@cs.umd.edu), 
;;;;  Department of Computer Science
;;;;  University of Maryland
;;;;  College Park, MD  20742
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Purpose:
;;;;
;;;;   This file contains macros and functions used to manipulate
;;;; data structures used for encoding conceptual dependency (CD)
;;;; assertions. In addition, there is a simple pattern matcher 
;;;; used to compare CD forms.
;;;;
;;;; Notes:
;;;;
;;;;   CD's are represented as a list of the form:
;;;;
;;;;       (<predicate> (<role-1> <filler-1>) ... (role-n filler-n))
;;;;
;;;; where <predicate> is a primitive CD act, <role-i> is an appropriate 
;;;; slot for that predicate, and <filler-i> an associated value.
;;;;
;;;;   The functions below manipulate this data structure.  A function 
;;;; name of the form "x-y" denotes a function applies to a data 
;;;; structure of type "y" and returns a data stucture of type "x".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Global Variables
;;;

(defconstant *cd-acts*
  '(atrans ptrans propel move grasp ingest
    expel mtrans conc mbuild attend speak)
  "List of valid CD predicates")

(defparameter *user-trace* t
  "Controls output from USER-TRACE function.  If T,
message to USER-TRACE is printed.  If NIL, no output
occurs.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Data Structures
;;;

;; HEADER-CD gets the head act of a CD form.
;; ROLES-CD gets the list of role-pairs of a CD form.

(defun header-cd (x)
   "Returns predicate of a CD form" ; eg PTRANS
   (car x))

(defun roles-cd (x) 
   "Returns list of (role value) pairs for
a CD form." ; eg ((ACTOR GIRL) (OBJECT KITE)), where role is OBJECT, value/filler is KITE etc
   (cdr x))

;; Role-pairs have the form (role filler) - ROLE-PAIR returns the
;; role and FILLER-PAIR returns the filler.

(defun role-pair (x) 
   "Returns the role of a (role filler) pair." ; eg if x = (OBJECT KITE), return OBJECT
   (car x))

(defun filler-pair (x) 
   "Returns the filler of a (role filler) pair." ; eg if x = (OBJECT KITE), return KITE
   (cadr x))

;; A filler for a role is found by looking for the role name
;; in the CD, and returning the filler if a pair is found.

(defun filler-role (role cd)
   "Returns the filler for a given role in CD, or
NIL if there is no filler for role."
   (let ((pair (assoc role (roles-cd cd))))
     ; break down the input CD into the list of (role filler) pairs that it's made up of, then get the pair (if any) associated with the inputted role. assign local variable pair to this pair
    (and pair (filler-pair pair)))) 
     ; if a value is found for pair, then return the filler of the pair

;; SETROLE makes a new CD form with (role filler) added or
;; replacing the old (role ...) pair.

(defun setrole (role filler cd)
   "Adds (role filler) pair to CD or changes the filler
for role if the role already is present."
  (cons (header-cd cd) ; cons'ing to the predicate of the inputted CD form because we are adding on to the list of role-filler pairs attached to this predicate
        (cons (list role filler) ; and what we're adding to the list of role-filler pairs for this CD is the inputted role-filler pair, which is its own list eg (OBJECT KITE)
              (remove role (roles-cd cd) :key #'role-pair)))) ; if there is already an existing role-filler pair for this role, remove the existing one and return the resulting cd structure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  CD Pattern Matcher
;;;

;; NOTE: I reimplemented variables as defstructs so we
;;       can have a print-function print the variable
;;       the same way it is read, i.e. % ?X => ?X

(defstruct (cd-var (:print-function print-cd-var)
                   (:predicate is-var))
  name) ; creates an object called cd-var, and each one has these functions

(defun print-cd-var (struct stream depth)
  (declare (ignore depth))
  (format stream "?~s" (cd-var-name struct))) ; pretty printing

(defun name-var (x) 
  (assert (is-var x) () "~s is not a CD variable." x)
  (cd-var-name x)) ; given a cd variable, return the cd var name (which are CLOS objects)

;; Read macro to convert variables of the form ?x to
;; a structure representing the variable.

(set-macro-character #\?
  #'(lambda (stream char)
      (make-cd-var :name (read stream t nil t)))
  t) ; when lisp interprets scripts, makes cd-var objects of symbols with ?
  
  ; cd var objects used in scripts to make easy matchings betw cd structures and script events

;;; MATCH takes three (predicate role-pair ...) forms as arguments.
;;; 1) a CD pattern which may contain variables;
;;; 2) a CD constant which contains no variables;
;;; 3) an optional binding form which specifies any bindings that
;;;    the variables in the pattern may already have.
;;;    The predicate of the binding form doesn't matter,
;;;    so T is used.  For convenience, MATCH also takes
;;;    NIL as a binding form and converts it to (T), which
;;;    is a binding form with no variables bound.  If no binding
;;;    form is specified, NIL is the default value.
;;; MATCH returns NIL only if the match failed. A match that
;;; succeeds but which involved no variables returns (T).

;;; For example, if the arguments were
;;; pattern = (PTRANS (ACTOR (*VAR* SHOPPER)) (TO (*VAR* STORE)))
;;; constant = (PTRANS (ACTOR (PERSON)) (TO (STORE)))
;;; binding = (T (SHOPPER (PERSON)) (STORE (STORE)))
;;; then the variables in the pattern are SHOPPER and STORE and
;;; the binding form says that these variables are bound to
;;; PERSON and STORE.
;;; The pattern matches the constant if the predicates are equal
;;; and if all of the roles in the pattern are matched by roles
;;; in the constant
;;; - a variable matches if its binding matches;
;;; - roles in the constant that are not in the pattern are ignored.

;;; MATCH returns either NIL if the match failed or an updated binding
;;; form that includes any new variable bindings that may have been
;;; produced.

;;; A NIL constant always matches - this means that the pattern
;;; (PERSON (NAME (JACK))) matches the constant (PERSON) even though
;;; the (NAME) is missing.

(defun match (pattern constant bindings)
  "Matches CD pattern to CD constant given bindings.
Returns (T (var val)*) if a match results, or NIL if
the pattern did not match the constant."
  (let ((binding-form (or bindings (list t)))) ; assigning a local variable binding-form; if bindings is non-nil, then initialize binding-form to a list containing t
    (cond ((or (null constant) (equal pattern constant))
           binding-form) 
		   ; if the inputted constant is null OR the inputted pattern and constant are equal, return binding-form, meaning there was a match. NOTE: the reason why we accept if constant is null is because there is nothing to 'match' so for our purpose we just assume it matches
          ((is-var pattern)
           (match-var pattern constant binding-form))
		   ; if the inputted pattern is a variable, call match-var to handle it specially
          ((or (atom constant) (atom pattern))
           nil) 
		   ; if both previous conditions failed, i.e. constant and pattern were not equal and pattern was not a variable, then if either constant or pattern is an atom, there is no match, so return nil
          ((eq (header-cd pattern) (header-cd constant)) 
           (match-args (roles-cd pattern) constant binding-form))))) 
		   ; if predicate of pattern and of constant are equal, then call match-args to loop over each role-filler pair in pattern and compare it against those of constant

;;; MATCH-ARGS takes a list of role pairs (a role pair has the form
;;; (role filler)), a constant CD form, and a binding form.
;;; It goes through the list of pairs and matches each pair against
;;; the corresponding role pair in the constant form - all of these
;;; must match.

(defun match-args (pattern-args constant bindings)
  (dolist (pattern-arg pattern-args) ; for every role-filler pair in pattern-args, which is a list of role-filler pairs
    (let ((const (filler-role (role-pair pattern-arg) constant)) ; get the role of the current role-filler pair, then for this role, get the corresponding filler in the constant CD - assign local variable const to the filler from constant
          (var (filler-pair pattern-arg))) ; assign local variable var to the filler from the current role-filler pair in the loop
        (setq bindings (match var const bindings)) ; see if var and const match, and assign bindings to the result
        (if (null bindings) ; if bindings is null, aka var and const did not match,
          (return nil)))) ; return nil
  bindings) ; return bindings

;;; MATCH-VAR takes a variable, a constant, and a binding form -
;;; if the variable has a binding then the binding must match the
;;; constant - otherwise the binding form is updated to bind the
;;; variable to the constant.


(defun match-var (pattern constant bindings)
  (let ((var-value (filler-role (name-var pattern) bindings))) ; name-var pattern gets the variable without question mark, filler-role gets the filler for a role in a list of role-filler pairs. ; e.g. bindings -> ($restaurant (diner (person (name (jack)))) (restaurant (restaurant)) (meal (lobster)))). pattern -> ?meal. (name-var pattern) -> meal. filler-role will find the filler bound to meal in bindings, which is lobster. var-value -> lobster
    (if var-value ;if non-nil, evaluate first form, else evaluate second form
      (match var-value constant bindings) ; if var-value has a value, that means the role specified by pattern already has an existing binding. see if the filler matches constant
      (cons-end bindings (list (name-var pattern) constant))))) ; otherwise, the pattern does not have a binding, so create a new binding between the role (name-var pattern) and the filler (constant)

;;; Instantiating Patterns
;;; The function INSTANTIATE takes a CD pattern and a binding 
;;; list, such as the one produced by MATCH, and returns a CD
;;; form with the variables in the pattern replaced by their 
;;; bindings.

(defun instantiate (cd-form bindings)
  "Replaces a CD with variables by a new CD with the
variables replaced by their corresponding values from
bindings."
  ; e.g. (ACTOR GIRL) (OBJECT PLANE) (TO SKY)
  (cond ((symbolp cd-form) cd-form) ; if cd-form is a symbol, return cd-form
        ((is-var cd-form) ; if cd-form is a variable, 
         (instantiate (filler-role (name-var cd-form) bindings) ; call instantiate recursively, this time on the filler found in bindings for the role in cd-form
                      bindings))
        (t (cons (header-cd cd-form) ; if cd-form is neither a symbol nor a variable, then cd-form is list of filler-role pairs. get the predicate of the cd-form and start cons'ing to it
                 (mapcar #'(lambda (pair) ; for each role-filler pair in cd-form,
                             (list (role-pair pair) ; make a list consisting of the role of the pair
                                   (instantiate (filler-pair pair) ; and the instantiation of the filler
                                                bindings)))
                         (roles-cd cd-form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Utility Functions
;;;

;; CONS-END is really inefficient!  Problem is some algorithms 
;; count on it being non-destructive.

(defun cons-end (l x)
  "Adds x to the end of list l"
  (append l (list x)))

(defun user-trace (str &rest args)
  (let ((*print-pretty* t))
    (when *user-trace*
      (apply #'format t str args))))

(provide :cd-functions)

