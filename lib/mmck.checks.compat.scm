;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Checks
;;;Contents: compatibility module
;;;Date: Mar 31, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines compatibility functions and syntaxes for code running on an
;;;	augmented R6RS language.
;;;
;;;Copyright (C) 2019 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms of the GNU  Lesser General Public License as published  by the Free Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
;;;
;;;You should  have received a  copy of the GNU  Lesser General Public  License along
;;;with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(declare (unit mmck.checks.compat)
	 (emit-import-library mmck.checks.compat))

(module (mmck.checks.compat)
    (call-with-string-output-port error assertion-violation)
  (import (scheme)
	  (chicken module)
	  (prefix (chicken condition) cnd::)
	  (only (chicken base)
		receive
		open-output-string
		get-output-string))
  (reexport (rename (only (chicken base)
			  fixnum?
			  when unless
			  case-lambda
			  current-error-port flush-output
			  exit)
		    (flush-output	flush-output-port)))


;;;; input/output

(define (call-with-string-output-port proc)
  (receive (port extract)
      (open-string-output-port)
    (proc port)
    (extract)))

(define (open-string-output-port)
  (let ((port (open-output-string)))
    (values port (lambda ()
		   (get-output-string port)))))


;;;; errors

(define (error who message . irritants)
  (cnd::abort
   (cnd::condition
    `(exn location ,who message ,message arguments ,irritants)
    '(error))))

(define (assertion-violation who message . irritants)
  (cnd::abort
   (cnd::condition
    `(exn location ,who message ,message arguments ,irritants)
    '(assertion-violation))))


;;;; done

#| end of module |# )

;;; end of file
