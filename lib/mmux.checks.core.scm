;;;This module is derived from SRFI 78 "Lightweight testing".
;;;
;;;Copyright (c) 2009-2016, 2019 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(declare (unit mmux.checks.core)
	 (uses mmux.checks.compat)
	 (emit-import-library mmux.checks.core))

(module (mmux.checks.core)
    (
     ;; bindings from the SRFI
     (syntax: check checks::eval-this-test? checks::proc checks::mode)
     check-report
     check-set-mode!
     check-reset!
     check-passed?

     ;; result handling
     (syntax: with-result) add-result get-result

     ;; more macros
     false-if-exception check-for-true check-for-false
     (syntax: check-for-assertion-violation)
     (syntax: check-for-procedure-argument-violation)
     (syntax: check-for-procedure-signature-argument-violation)
     (syntax: check-for-procedure-signature-return-value-violation)
     (syntax: check-for-procedure-arguments-consistency-violation)
     (syntax: check-for-expression-return-value-violation)

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
			make-parameter parameterize)
		  (parameterize	parametrise))
	  (rename (only (chicken pretty-print)
			pretty-print)
		  (pretty-print	chicken::pretty-print))
	  (rename (only (chicken process-context)
			get-environment-variable)
		  (get-environment-variable	getenv))
	  (mmux.checks.compat))


;;; utilities

(define check-quiet-tests?
  (let ((S (getenv "CHECKS_QUIET")))
    (and S
	 (or (and (fixnum? S)
		  (not (zero? S)))
	     (string=? S "yes")))))

(define (check-display thing)
  (unless check-quiet-tests?
    (chicken::display thing (current-error-port))))

(define (check-write thing)
  (unless check-quiet-tests?
    (chicken::write thing (current-error-port))))

(define (check-newline)
  (unless check-quiet-tests?
    (chicken::newline (current-error-port))))

(define (check-pretty-print thing)
  (unless check-quiet-tests?
    (chicken::pretty-print thing (current-error-port))))

(define (flush-checks-port)
  (unless check-quiet-tests?
    (flush-output-port (current-error-port))))

(define check-pretty-print/no-trailing-newline
  (case-lambda
   ((datum output-port)
    (unless check-quiet-tests?
      (let* ((os	(call-with-string-output-port
			    (lambda (sop)
			      (parametrise ((current-error-port sop))
				(check-pretty-print datum)))))
	     (len	(string-length os))
	     (os	(if (and (positive? len)
				 (char=? #\newline
					 (string-ref os (- len 1))))
			    (substring os 0 (- len 1))
			  os)))
	(parametrise ((current-error-port output-port))
	  (check-display os)))))
   ((datum)
    (check-pretty-print/no-trailing-newline datum (current-error-port)))))

(define (string-prefix? prefix the-string)
  (or (eq? prefix the-string)
      (let ((prelen (string-length prefix)))
	(and (<= prelen (string-length the-string))
	     (string=? prefix (substring the-string 0 prelen))))))

(define (string-suffix? suffix the-string)
  (or (eq? suffix the-string)
      (let ((strlen (string-length the-string))
	    (suflen (string-length suffix)))
	(and (<= suflen strlen)
	     (string=? suffix (substring the-string suflen strlen))))))



;;; mode handling

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

(define (check-set-mode! mode)
  (checks::mode mode))


;;; state handling

(define check:correct 0)
(define check:failed '())

(define (check-reset!)
  (set! check:correct 0)
  (set! check:failed '()))

(define (check:add-correct!)
  (set! check:correct (+ 1 check:correct)))

(define (check:add-failed! expression actual-result expected-result)
  (set! check:failed
	(cons (list expression actual-result expected-result)
	      check:failed)))


;;; reporting

(define (check:report-expression expression)
  (check-newline)
  (check-pretty-print expression)
  (check-display " => "))

(define (check:report-actual-result actual-result)
  (check-pretty-print actual-result)
  (check-display " ; "))

(define (check:report-correct cases)
  (check-display "correct")
  (if (not (= cases 1))
      (begin
	(check-display " (")
	(check-display cases)
	(check-display " cases checked)")))
  (check-newline))

(define (check:report-failed expected-result)
  (check-display "*** failed ***")
  (check-newline)
  (check-display " ; expected result: ")
  (check-pretty-print expected-result)
  (check-newline)
  (flush-checks-port))

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

(define (check-passed? expected-total-count)
  (and (= (length check:failed) 0)
       (= check:correct expected-total-count)))


;;; simple checks

(define (checks::proc expression thunk equal expected-result)
  (case (checks::mode)
    ((0)
     (values))
    ((1)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
	   (check:add-correct!)
	 (check:add-failed! expression actual-result expected-result)))
     (values))
    ((10)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
	   (check:add-correct!)
	 (begin
	   (check:report-expression expression)
	   (check:report-actual-result actual-result)
	   (check:report-failed expected-result)
	   (check:add-failed! expression actual-result expected-result))))
     (values))
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
     (values))
    (else
     (values))))

(define-syntax srfi:check
  (syntax-rules (=>)
    ((_ ?expr => ?expected)
     (srfi:check ?expr (=> equal?) ?expected))
    ((_ ?expr (=> ?equal) ?expected)
     (when (>= (checks::mode) 1)
       (checks::proc (quote ?expr) (lambda () ?expr) ?equal ?expected)))))


;;;; handling results

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

(define (add-result value)
  (result (cons value (result)))
  value)

(define (get-result)
  (reverse (result)))


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
      (guard (exc (else #f))
	(with-ignored-warnings
	 ?form0 ?form ...)))))

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

(define-syntax check-for-assertion-violation
  (syntax-rules (=>)
    ((_ ?body => ?expected-who/irritants)
     (check
	 (guard (E ((assertion-violation? E)
		    (list (condition-who E)
			  (condition-irritants E)))
		   (else E))
	   (with-ignored-warnings ?body))
       => ?expected-who/irritants))
    ))

(define-syntax check-for-procedure-argument-violation
  (syntax-rules (=>)
    ((_ ?body => ?expected-who/irritants)
     (check
	 (guard (E ((procedure-argument-violation? E)
		    (list (condition-who E)
			  (condition-irritants E)))
		   (else E))
	   (with-ignored-warnings ?body))
       => ?expected-who/irritants))
    ))

(define-syntax check-for-procedure-signature-argument-violation
  (syntax-rules (=>)
    ((_ ?body => ?expected-who/irritants)
     (check
	 (guard (E ((procedure-signature-argument-violation? E)
		    (list (condition-who E)
			  (procedure-signature-argument-violation.one-based-argument-index E)
			  (procedure-signature-argument-violation.failed-expression E)
			  (procedure-signature-argument-violation.offending-value E)))
		   (else E))
	   (with-ignored-warnings ?body))
       => ?expected-who/irritants))
    ))

(define-syntax check-for-procedure-signature-return-value-violation
  (syntax-rules (=>)
    ((_ ?body => ?expected-who/irritants)
     (check
	 (guard (E ((procedure-signature-return-value-violation? E)
		    (list (condition-who E)
			  (procedure-signature-return-value-violation.one-based-return-value-index E)
			  (procedure-signature-return-value-violation.failed-expression E)
			  (procedure-signature-return-value-violation.offending-value E)))
		   (else E))
	   (with-ignored-warnings ?body))
       => ?expected-who/irritants))
    ))


;;; --------------------------------------------------------------------

(define-syntax check-for-procedure-arguments-consistency-violation
  (syntax-rules (=>)
    ((_ ?body => ?expected-who/irritants)
     (check
	 (guard (E ((procedure-arguments-consistency-violation? E)
		    (list (condition-who E)
			  (condition-irritants E)))
		   (else E))
	   (with-ignored-warnings ?body))
       => ?expected-who/irritants))
    ))

(define-syntax check-for-expression-return-value-violation
  (syntax-rules (=>)
    ((_ ?body => ?expected-who/irritants)
     (check
	 (guard (E ((expression-return-value-violation? E)
		    (list (condition-who E)
			  (condition-irritants E)))
		   (else E))
	   (with-ignored-warnings ?body))
       => ?expected-who/irritants))
    ))


;;;; selecting tests

(define check-test-name
  (make-parameter #f
    (lambda (value)
      (unless (or (not value) (string? value) (symbol? value))
	(assertion-violation 'check-test-name
	  "expected #f or string as parameter value" value))
      (if (symbol? value)
	  (symbol->string value)
	value))))

(define selected-test
  (getenv "CHECKS_NAME"))

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
