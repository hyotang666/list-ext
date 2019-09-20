(defpackage :list-ext.spec
  (:use :cl :jingoh :list-ext))
(in-package :list-ext.spec)
(setup :list-ext)

(requirements-about NLAST)

;;;; Description:
; BUTLAST has NBUTLAST, now LAST has NLAST. See CL:LAST.
; Return last cons of LIST.
#?(nlast '(1 2 3 4 5))
:values ((5)
	 (1 2 3 4))
#+syntax
(NLAST list &optional (num 1)) ; => result

;;;; Arguments and Values:

; list := list

; num := (INTEGER 0 *), otherwise error.
; Specify how many conses returned.
#?(nlast '(1 2 3 4 5) 2)
:values ((4 5)
	 (1 2 3))
#?(nlast '(1 2 3 4 5) 0)
:values (NIL
	  (1 2 3 4 5))
; When NUM is over the length of LIST, LIST is returned.
#?(nlast '(1 2 3 4 5) 8)
:values ((1 2 3 4 5)
	 NIL)
#?(nlast '(1 2 3 4 5) -1) :signals error
,:lazy T
#?(nlast '(1 2 3 4 5) nil) :signals type-error
,:lazy T

; result := list

;;;; Affected By:
; none

;;;; Side-Effects:
; LIST is destructively modified.
#?(let((list (list 1 2 3 4 5)))
    (nlast list)
    list)
=> (1 2 3 4)
,:test equal

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SAME-ELT-P)

;;;; Description:

#+syntax
(SAME-ELT-P list &key (test #'eql) (key #'identity)) ; => result

; Test that LIST's all element is same or not.
#?(same-elt-p '(1 2 3 4)) => NIL
#?(same-elt-p '(1 1 1 1)) => T
;;;; Arguments and Values:

; list := list

; test := function, the default is eql.
#?(same-elt-p (list (princ-to-string 2) "2" "2")) => NIL
#?(same-elt-p (list (princ-to-string 2) "2" "2"):test #'string=) => T

; key := function
#?(same-elt-p (list (princ-to-string 2) "2" "2")) => NIL
#?(same-elt-p (list (princ-to-string 2) "2" "2"):key #'parse-integer) => T

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FIND-NTH)

;;;; Description:
; Combine CL:FIND and CL:NTH.

#+syntax
(FIND-NTH target nth list &key (test #'eql) (key #'identity) (start 0) end from-end) ; => result

#?(find-nth #\A 0 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0)))
=> A0
#?(find-nth #\A 1 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0)))
=> A1
#?(find-nth #\A 2 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0)))
=> A2
#?(find-nth #\A 3 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0)))
=> A3
#?(find-nth #\A 4 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0)))
=> NIL
;;;; Arguments and Values:

; target := T

; nth := (integer 0 *), otherwise error
#?(find-nth #\A -1 '(a0 a1 a2 a3) :key(lambda(x)(char(string x)0)))
:signals error
,:lazy T
#?(find-nth #\A NIL '(a0 a1 a2 a3) :key(lambda(x)(char(string x)0)))
:signals type-error
,:lazy T
; When specified NTH over, NIL is returned.
#?(find-nth #\A 4 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0)))
=> NIL
; When nth = 0, semantically equals to CL:FIND.
#?(find-nth #\A 0 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0)))
:equivalents
(find #\A '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0)))

; list := list, otherwise error.
#?(find-nth #\A 2 #(a0 a1 a2 a3) :key (lambda(x)(char(string x)0)))
:signals type-error
,:lazy T

; test := function, the default is #'EQL.
#?(find-nth "A" 0 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0)))
=> NIL
#?(find-nth "A" 0 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0))
	    :test #'string=)
=> A0

; key := function, the default is #'IDENTITY
#?(find-nth #\A 0 '(a0 a1 a2 a3))
=> NIL
#?(find-nth #\A 0 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0)))
=> A0

; start := (integer * 0)
#?(find-nth #\A 0 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0))
	    :start 1)
=> A1
; When START is over the length, NIL is returned.
#?(find-nth #\A 0 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0))
	    :start 8)
=> NIL

; end := (integer * 0) Must greater equal than START.
; When lesser than START, an error is signaled.
#?(find-nth #\A 0 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0))
	    :start 2 :end 1)
:signals error
; When over the length, NIL is returned.
#?(find-nth #\A 8 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0))
	    :end 8)
=> NIL

; from-end := boolean
#?(find-nth #\A 0 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0))
	    :from-end t)
=> A3
#?(find-nth #\A 1 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0))
	    :from-end t)
=> A2
#?(find-nth #\A 2 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0))
	    :from-end t)
=> A1
#?(find-nth #\A 3 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0))
	    :from-end t)
=> A0
#?(find-nth #\A 4 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0))
	    :from-end t)
=> NIL

; result := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; START and END specifies sublist of LIST.
#?(find-nth #\A 0 '(a0 a1 a2 a3) :key (lambda(x)(char(string x)0))
	    :start 1 :end 3 :from-end T)
:equivalents
(find #\A (subseq '(a0 a1 a2 a3) 1 3)
      :key (lambda(x)(char(string x)0))
      :from-end T)

;;;; Exceptional-Situations:

;;;; Examples:
(requirements-about SIEVE)

;;;; Description:
; Like filter, but return two values.
; SAMES and DIFFS.
#?(sieve 1 '(1 2 3 1 2 3))
:values ((1 1)
	 (2 3 2 3))

#+syntax
(SIEVE item list &key (test #'eql) (key #'identity)) ; => result

;;;; Arguments and Values:

; item := T

; list := List, otherwise error.
#?(sieve 1 #(1 2 3 1 2 3)) :signals type-error

; test := Function, the default is #'EQL
#?(sieve (princ-to-string 1) '("1" "2" "3" "1" "2" "3"))
:values (()
	 ("1" "2" "3" "1" "2" "3"))
#?(sieve "1" '("1" "2" "3" "1" "2" "3"):test #'string=)
:values (("1" "1")
	 ("2" "3" "2" "3"))

; key := Function, the default is #'IDENTITY
#?(sieve 1 '("1" "2" "3" "1" "2" "3"))
:values (()
	 ("1" "2" "3" "1" "2" "3"))
#?(sieve 1 '("1" "2" "3" "1" "2" "3"):key #'parse-integer)
:values (("1" "1")
	 ("2" "3" "2" "3"))

; result := (values sames diffs)

; sames := list

; diffs := list

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SIEVE-IF)

;;;; Description:
; Sibling of SIEVE.

#+syntax
(SIEVE-IF pred list &key (key #'identity)) ; => result
#?(sieve-if #'symbolp '(a 1 b 2 "HOGE"))
:values ((A B)
	 (1 2 "HOGE"))

;;;; Arguments and Values:

; pred := function.

; list := list, otherwise error.
#?(sieve-if #'symbolp #(a 1 b 2 "HOGE")) :signals type-error

; key := function, the default is #'IDENTITY.
#?(sieve-if #'symbolp '((a) (1) (b) (2) ("HOGE"))
	    :key #'car)
:values (((A) (B))
	 ((1) (2) ("HOGE")))

; result := (values sames diffs)

; sames := list

; diffs := list

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

