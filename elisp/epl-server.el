;; epl-server.el -- run as a slave of Perl
;; Copyright (C) 2001 by John Tobey.  All rights reserved.

;; This library is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


(require 'epl)

(setq epl-interp
      (vector 'perl-interpreter-tag
	      nil ;in
	      nil ;out
	      (generate-new-buffer "perl") ;buffer
	      (make-hash-table :test 'eq) ;gcpro
	      1   ;next-handle
	      (make-hash-table :test 'eq :weakness 'value) ;refs
	      0   ;nrefs
	      'ready ;status
	      ))

(setq epl-interp-map (make-hash-table :test 'eq))
(puthash t epl-interp epl-interp-map)
(add-hook 'kill-emacs-hook 'epl-kill-emacs-hook)

(let ((standard-input t)) (epl-loop))
