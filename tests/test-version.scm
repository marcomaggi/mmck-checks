;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMUX CHICKEN Checks
;;;Contents: test program for version functions
;;;Date: Mar 31, 2019
;;;
;;;Abstract
;;;
;;;	This program tests version functions.
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

(require-library (mmux checks))

(module (test-version)
    ()
  (import (scheme)
	  (prefix (mmux checks) checks::)
	  (chicken pretty-print))


;;;; stuff

(pretty-print (list 'version-string		(checks::mmux-checks-version-string)))
(pretty-print (list 'version-interface-current	(checks::mmux-checks-version-interface-current)))
(pretty-print (list 'version-interface-revision	(checks::mmux-checks-version-interface-revision)))
(pretty-print (list 'version-interface-age	(checks::mmux-checks-version-interface-age)))


;;;; done

#| end of module |# )

;;; end of file
