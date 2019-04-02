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
	  (mmux checks)
	  (chicken pretty-print))


;;;; stuff

(pretty-print (list 'mmux-checks-package-major-version (mmux-checks-package-major-version)))
(pretty-print (list 'mmux-checks-package-minor-version (mmux-checks-package-minor-version)))
(pretty-print (list 'mmux-checks-package-patch-level (mmux-checks-package-patch-level)))
(pretty-print (list 'mmux-checks-package-prerelease-tag (mmux-checks-package-prerelease-tag)))
(pretty-print (list 'mmux-checks-package-build-metadata (mmux-checks-package-build-metadata)))
(pretty-print (list 'mmux-checks-package-version (mmux-checks-package-version)))
(pretty-print (list 'mmux-checks-package-semantic-version (mmux-checks-package-semantic-version)))


;;;; done

#| end of module |# )

;;; end of file
