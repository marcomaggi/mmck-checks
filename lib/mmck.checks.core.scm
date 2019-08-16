;;;This module is derived from SRFI 78 "Lightweight testing".
;;;
;;;Copyright (c) 2009-2016, 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;Copyright (c) 2005-2006 Sebastian Egner <Sebastian.Egner@philips.com>
;;;Modified by Derick Eddington for R6RS Scheme.
;;;Modified by Marco Maggi for CHICKEN.
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software  and associated documentation  files (the ``Software''), to  deal in
;;;the Software without restriction, including  without limitation the rights to use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED ``AS  IS'', WITHOUT  WARRANTY OF  ANY KIND,  EXPRESS OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND  NONINFRINGEMENT. IN NO  EVENT SHALL THE  AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(declare (unit mmck.checks.core)
	 (uses mmck.checks.compat)
	 (emit-import-library mmck.checks.core))

(module (mmck.checks.core)
    (
     ;; bindings from the SRFI
     (syntax: check checks::eval-this-test? checks::proc checks::mode)
     check-report
     check-set-mode!
     check-reset!
     check-passed?

     ;; result handling
     (syntax: with-result call-with-values append list get-result result)
     add-result get-result

     ;; more macros
     false-if-exception check-for-true check-for-false

     ;; selecting tests
     check-test-name

     ;; output
     check-quiet-tests?
     check-display
     check-write
     check-newline
     check-pretty-print)
  (import (chicken module)
	  (rename (scheme)
		  (display	chicken::display)
		  (write	chicken::write)
		  (newline	chicken::newline))
	  (rename (only (chicken base)
			declare
			make-parameter
			parameterize
			void)
		  (parameterize	parameterise))
	  (rename (only (chicken pretty-print)
			pretty-print)
		  (pretty-print	chicken::pretty-print))
	  (rename (only (chicken process-context)
			get-environment-variable)
		  (get-environment-variable	getenv))
	  (only (chicken condition)
		abort
		condition
		condition-case)
	  (only (chicken type)
		:)
	  (mmck.checks.compat))


;;; utilities

(: check-quiet-tests? boolean)
(define check-quiet-tests?
  (let ((S (getenv "CHECKS_QUIET")))
    (and S
	 (or (and (fixnum? S)
		  (not (zero? S)))
	     (string=? S "yes")))))

(: check-display (* -> undefined))
(define (check-display thing)
  (unless check-quiet-tests?
    (chicken::display thing (current-error-port))))

(: check-write (* -> undefined))
(define (check-write thing)
  (unless check-quiet-tests?
    (chicken::write thing (current-error-port))))

(: check-newline (-> undefined))
(define (check-newline)
  (unless check-quiet-tests?
    (chicken::newline (current-error-port))))

(: check-pretty-print (* -> undefined))
(define (check-pretty-print thing)
  (unless check-quiet-tests?
    (chicken::pretty-print thing (current-error-port))))

(: flush-checks-port (-> undefined))
(define (flush-checks-port)
  (unless check-quiet-tests?
    (flush-output-port (current-error-port))))

(declare (enforce-argument-types check-pretty-print/no-trailing-newline))
(: check-pretty-print/no-trailing-newline (* #!optional output-port -> undefined))
(define check-pretty-print/no-trailing-newline
  (case-lambda
   ((datum output-port)
    (unless check-quiet-tests?
      (let* ((os	(call-with-string-output-port
			    (lambda (sop)
			      (parameterise ((current-error-port sop))
				(check-pretty-print datum)))))
	     (len	(string-length os))
	     (os	(if (and (positive? len)
				 (char=? #\newline
					 (string-ref os (- len 1))))
			    (substring os 0 (- len 1))
			  os)))
	(parameterise ((current-error-port output-port))
	  (check-display os)))))
   ((datum)
    (check-pretty-print/no-trailing-newline datum (current-error-port)))))

(declare (enforce-argument-types string-prefix?))
(: string-prefix? (string string -> boolean))
(define (string-prefix? prefix the-string)
  (or (eq? prefix the-string)
      (let ((prelen (string-length prefix)))
	(and (<= prelen (string-length the-string))
	     (string=? prefix (substring the-string 0 prelen))))))

(declare (enforce-argument-types string-suffix?))
(: string-suffix? (string string -> boolean))
(define (string-suffix? suffix the-string)
  (or (eq? suffix the-string)
      (let ((strlen (string-length the-string))
	    (suflen (string-length suffix)))
	(and (<= suflen strlen)
	     (string=? suffix (substring the-string suflen strlen))))))



;;; mode handling

(declare (type (checks:mode (procedure (#!optional symbol) . (fixnum)))))
(define checks::mode
  ;;This  MAKE-PARAMETER call  will pass  its first  argument through  the validation
  ;;function in its second parameter.
  (make-parameter 'report
    (lambda (v)
      (case v
	((off)           0)
	((summary)       1)
	((report-failed) 10)
	((report)        100)
	(else (error 'checks::mode "unrecognized mode for CHECKS::MODE" v))))))

(declare (enforce-argument-types check-set-mode!))
(: check-set-mode! (symbol -> fixnum))
(define (check-set-mode! mode)
  (checks::mode mode))


;;; state handling

(: check:correct fixnum)
(: check:failed  list)
(define check:correct 0)
(define check:failed '())

(: check-reset! (-> undefined))
(define (check-reset!)
  (set! check:correct 0)
  (set! check:failed '()))

(: check:add-correct! (-> undefined))
(define (check:add-correct!)
  (set! check:correct (+ 1 check:correct)))

(: check:add-failed! (* * * -> undefined))
(define (check:add-failed! expression actual-result expected-result)
  (set! check:failed
	(cons (list expression actual-result expected-result)
	      check:failed)))


;;; reporting

(: check:report-expression (* -> undefined))
(define (check:report-expression expression)
  (check-newline)
  (check-pretty-print expression)
  (check-display " => "))

(: check:report-actual-result (* -> undefined))
(define (check:report-actual-result actual-result)
  (check-pretty-print actual-result)
  (check-display " ; "))

(declare (enforce-argument-types check:report-correct))
(: check:report-correct (fixnum -> undefined))
(define (check:report-correct cases)
  (check-display "correct")
  (if (not (= cases 1))
      (begin
	(check-display " (")
	(check-display cases)
	(check-display " cases checked)")))
  (check-newline))

(: check:report-failed (* -> undefined))
(define (check:report-failed expected-result)
  (check-display "*** failed ***")
  (check-newline)
  (check-display " ; expected result: ")
  (check-pretty-print expected-result)
  (check-newline)
  (flush-checks-port))

(: check-report (-> undefined))
(define (check-report)
  (when (>= (checks::mode) 1)
    (check-newline)
    (check-display "; *** checks *** : ")
    (check-display check:correct)
    (check-display " correct, ")
    (check-display (length check:failed))
    (check-display " failed.")
    (when (> (checks::mode) 1)
      (if (pair? check:failed)
	  (let* ((ell	(reverse check:failed))
		 (w	(car ell))
		 (expression		(car w))
		 (actual-result		(cadr w))
		 (expected-result	(caddr w)))
	    (check-display " First failed example:")
	    (check-newline)
	    (check:report-expression expression)
	    (check:report-actual-result actual-result)
	    (check:report-failed expected-result)
	    (check-newline)
	    (check-newline))
	(begin
	  (check-newline)
	  (check-newline)
	  (check-newline)))
      (flush-output-port (current-error-port))))
  (cond ((null? check:failed)
	 ;;Success for GNU Automake.
	 (exit 0))
	((not (null? check:failed))
	 ;;Test failure for GNU Automake.
	 (exit 1))
;;;	((something)
;;;	 ;;Skipped test for GNU Automake.
;;;	 (exit 77))
	(else
	 ;;Hard error for GNU Automake.
	 (exit 99))))

(declare (enforce-argument-types check-passed?))
(: check-passed? (fixnum -> boolean))
(define (check-passed? expected-total-count)
  (and (= (length check:failed) 0)
       (= check:correct expected-total-count)))


;;; simple checks

(declare (enforce-argument-types checks::proc))
(: checks::proc (forall ((?thunk	(-> *))
			 (?equal	(* * -> boolean)))
			(procedure (* ?thunk ?equal *) . (undefined))))
(define (checks::proc expression thunk equal expected-result)
  (case (checks::mode)
    ((0)
     (void))
    ((1)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
	   (check:add-correct!)
	 (check:add-failed! expression actual-result expected-result)))
     (void))
    ((10)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
	   (check:add-correct!)
	 (begin
	   (check:report-expression expression)
	   (check:report-actual-result actual-result)
	   (check:report-failed expected-result)
	   (check:add-failed! expression actual-result expected-result))))
     (void))
    ((100)
     (check:report-expression expression)
     (let ((actual-result (thunk)))
       (check:report-actual-result actual-result)
       (if (equal actual-result expected-result)
	   (begin (check:report-correct 1)
		  (check:add-correct!))
	 (begin (check:report-failed expected-result)
		(check:add-failed! expression
				   actual-result
				   expected-result))))
     (void))
    (else
     (void))))

(define-syntax srfi:check
  (syntax-rules (=>)
    ((_ ?expr => ?expected)
     (srfi:check ?expr (=> equal?) ?expected))
    ((_ ?expr (=> ?equal) ?expected)
     (when (>= (checks::mode) 1)
       (checks::proc (quote ?expr) (lambda () ?expr) ?equal ?expected)))))


;;;; handling results

(declare (type (result (procedure (#!optional *) . (*)))))
(define result
  (make-parameter #f))

(define-syntax with-result
  (syntax-rules ()
    ((_ ?form ... ?last-form)
     (parameterize ((result '()))
       ?form ...
       (call-with-values
	   (lambda () ?last-form)
	 (lambda args
	   (append args (list (get-result)))))))))

(: add-result (* -> *))
(define (add-result value)
  (result (cons value (result)))
  value)

(: get-result (-> list))
(define (get-result)
  (reverse (result)))

(define-syntax values->list
  (syntax-rules ()
    ((_ ?expr)
     (call-with-values
	 (lambda () ?expr)
       list))
    ))


;;;; more macros

(define-syntax with-ignored-warnings
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (with-exception-handler
	 (lambda (E)
	   (unless (warning? E)
	     (raise E)))
       (lambda ()
	 ?body0 ?body ...)))
    ))

(define-syntax false-if-exception
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (condition-case (with-ignored-warnings ?form0 ?form ...)
       (() #f)))
    ))

(define-syntax check-for-true
  (syntax-rules ()
    ((_ ?form)
     (check (if ?form #t #f) => #t))
    ((_ (quote ?name) ?form)
     (check (quote ?name) (if ?form #t #f) => #t))))

(define-syntax check-for-false
  (syntax-rules ()
    ((_ ?form)
     (check (if ?form #t #f) => #f))
    ((_ (quote ?name) ?form)
     (check (quote ?name) (if ?form #t #f) => #f))))


;;;; selecting tests

(: check-test-name (#!optional (or false string symbol) -> (or false string symbol)))
(define check-test-name
  (make-parameter #f
    (lambda (value)
      (unless (or (not value) (string? value) (symbol? value))
	(assertion-violation 'check-test-name
	  "expected #f or string as parameter value" value))
      (if (symbol? value)
	  (symbol->string value)
	value))))

(: selected-test (or false string))
(define selected-test
  (getenv "CHECKS_NAME"))

(: checks::eval-this-test? (-> boolean))
(define (checks::eval-this-test?)
  (or (not selected-test)
      (zero? (string-length selected-test))
      (let ((name (check-test-name)))
	(if name
	    (or (string-prefix? selected-test name)
		(string-suffix? selected-test name))
	  #f))))

(define-syntax check
  (syntax-rules (=>)

    ((_ ?expr => ?expected-result)
     (check ?expr (=> equal?) ?expected-result))

    ((_ ?expr (=> ?equal) ?expected-result)
     (when (checks::eval-this-test?)
       (srfi:check ?expr (=> ?equal) ?expected-result)))

    ((_ ?name ?expr => ?expected-result)
     (check ?name ?expr (=> equal?) ?expected-result))

    ((_ ?name ?expr (=> ?equal) ?expected-result)
     (parameterize ((check-test-name ?name))
       (when (checks::eval-this-test?)
	 (srfi:check ?expr (=> ?equal) ?expected-result))))

    ;;; multiple values

    ((_ ?expr => ?expected-result0 ?expected-result1 ?expected-result ...)
     (check ?expr (=> equal?) ?expected-result0 ?expected-result1 ?expected-result ...))

    ((_ ?expr (=> ?equal) ?expected-result0 ?expected-result1 ?expected-result ...)
     (when (checks::eval-this-test?)
       (srfi:check (values->list ?expr) (=> ?equal) (list ?expected-result0 ?expected-result1 ?expected-result ...))))

    ((_ ?name ?expr => ?expected-result0 ?expected-result1 ?expected-result ...)
     (check ?name ?expr (=> equal?) ?expected-result0 ?expected-result1 ?expected-result ...))

    ((_ ?name ?expr (=> ?equal) ?expected-result0 ?expected-result1 ?expected-result ...)
     (parameterize ((check-test-name ?name))
       (when (checks::eval-this-test?)
	 (srfi:check (values->list ?expr) (=> ?equal) (list ?expected-result0 ?expected-result1 ?expected-result ...)))))
    ))

;; (define-syntax check
;;   (syntax-rules (=>)
;;     ((_ ?expr => ?expected-result)
;;      (check ?expr (=> equal?) ?expected-result))

;;     ((_ ?expr (=> ?equal) ?expected-result)
;;      (when (checks::eval-this-test?)
;;        (srfi:check ?expr (=> ?equal) ?expected-result)))

;;     ((_ ?name ?expr => ?expected-result)
;;      (check ?name ?expr (=> equal?) ?expected-result))

;;     ((_ ?name ?expr (=> ?equal) ?expected-result)
;;      (parameterize ((check-test-name ?name))
;;        (when (checks::eval-this-test?)
;; 	 (srfi:check ?expr (=> ?equal) ?expected-result))))))


;;;; done

#| end of module |# )

;;; end of file
