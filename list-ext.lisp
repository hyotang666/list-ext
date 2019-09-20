(defpackage :list-ext(:use :cl :type-ext)
  (:export
    #:nlast
    #:same-elt-p
    #:find-nth
    #:sieve
    #:sieve-if
    ))
(in-package :list-ext)

(prototype nlast(list &optional (integer 0 *))
	   (values list list))
(defun nlast(list &optional(num 1))
  "BUTLAST has NBUTLAST, now LAST has NLAST. See CL:LAST."
  (if(zerop num)
    (values nil list)
    (%nlast list num)))
(defun %nlast(list num)
  (labels((OVERFLOW-CHECK(count)
	    (if(<= count 0)
	      (values list nil)
	      (REC list count)))
	  (REC(pointer count)
	    (if(= 1 count)
	      (values(cdr pointer)(progn (rplacd pointer nil)
					 list))
	      (REC(cdr pointer)(1- count)))))
    (OVERFLOW-CHECK(- (list-length list)num))))
(define-compiler-macro nlast(&whole whole list &optional (num 1)
				    &environment env)
  (if(constantp list env)
    (if(constantp num env)
      (multiple-value-bind(a b)(eval whole)
	`(VALUES ',a ',b))
      whole)
    (if(constantp num env)
      (if(zerop num)
	`(VALUES NIL ,list)
	`(%NLAST ,list ,num))
      whole)))

(prototype same-elt-p (list &key(:test function)(:key function))
	   (values boolean))
(defun same-elt-p(list &key(test #'eql)(key #'identity))
  (let((x(funcall key (car list))))
    (every (lambda(y)
	     (funcall test x (funcall key y)))
	   (cdr list))))

(prototype find-nth(T (integer 0 *) list
		      &key
		      (:test function)
		      (:key function)
		      (:start(or null (integer 0 *)))
		      (:end (or null (integer 0 *)))
		      (:from-end boolean))
	   T)

(defun find-nth(target nth list &key (test #'eql)(key #'identity)
		       (start 0) end from-end)
  #+(or ccl clisp) (assert(not(minusp nth)))
  (when end
    (assert(<= start end)))
  (labels((REC(list &optional (count 0))
	    (unless(endp list)
	      (BODY (car list)
		    count
		    (lambda(count)(REC (cdr list) count)))))
	  (TARGETP(elt)
	    (funcall test target(funcall key elt)))
	  (REC-WITH-END(list end &optional(count 0))
	    (unless(or (endp list)
		       (= start end))
	      (BODY (car list)
		    count
		    (lambda(count)(REC-WITH-END(cdr list)(1- end)count)))))
	  (BODY(elt count cont)
	    (if(TARGETP elt)
	      (if(= count nth)
		elt
		(funcall cont (1+ count)))
	      (funcall cont count)))
	  )
    (if end
      (if from-end
	(REC(nreverse(subseq list start end)))
	(REC-WITH-END(nthcdr start list)end))
      (if from-end
	(REC(reverse(nthcdr start list)))
	(REC(nthcdr start list))))))

(Prototype sieve(T list &key (:test function)(:key function))
	   (values list list))
(defun sieve(item list &key (test #'eql)(key #'identity))
  (%sieve (lambda(elt)(funcall test item (funcall key elt)))
	  list))
(defun %sieve(pred list)
  (loop :for elt :in list
	:if(funcall pred elt)
	:collect elt :into sames
	:else :collect elt :into diffs
	:finally (return (values sames diffs))))
(define-compiler-macro sieve(&whole whole item list &key test key)
  (if test
    (if key
      whole
      `(%SIEVE(LAMBDA(ELT)(FUNCALL ,test ,item ELT)),list))
    (if key
      `(%SIEVE(LAMBDA(ELT)(EQL ,item (FUNCALL ,key ELT))),list)
      `(%SIEVE(LAMBDA(ELT)(EQL ,item ELT)),list))))

(Prototype sive-if(function list &key(:key function))
	   (values list list))
(defun sieve-if(pred list &key(key #'identity))
  (loop :for elt :in list
	:if (funcall pred (funcall key elt))
	:collect elt :into sames
	:else :collect elt :into diffs
	:finally(return(values sames diffs))))
