;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMUX CHICKEN Checks
;;;Contents: version functions
;;;Date: Mar 31, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines version functions.
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

(declare (unit mmux.checks.version)
	 (emit-import-library mmux.checks.version))

(module (mmux.checks.version)
    *
  (import (scheme)
    (prefix mmux.checks.config config::))


;;;; version functions

(define (mmux-checks-version-string)
  config::mmux_chicken_checks_VERSION_INTERFACE_STRING)

(define (mmux-checks-version-interface-current)
  config::mmux_chicken_checks_VERSION_INTERFACE_CURRENT)

(define (mmux-checks-version-interface-revision)
  config::mmux_chicken_checks_VERSION_INTERFACE_REVISION)

(define (mmux-checks-version-interface-age)
  config::mmux_chicken_checks_VERSION_INTERFACE_AGE)


;;;; done

#| end of module |# )

;;; end of file
