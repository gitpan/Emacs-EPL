# Makefile generator for EPL.
# Usage: perl Makefile.PL

# Copyright (C) 2001 by John Tobey,
# jtobey@john-edwin-tobey.org.  All rights reserved.
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; see the file COPYING.  If not, write to the
#   Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
#   MA 02111-1307  USA

use ExtUtils::MakeMaker;

WriteMakefile (
	       'NAME'  => 'Emacs::EPL',
	       'VERSION' => get_version_from_el (),
	      );

warn ("
***NOTE

To run `make test', you need GNU Emacs 21.  Copy the files in the elisp
directory to where Emacs can find them, and set the EMACS environment
variable to Emacs' invocation name if other than `emacs'.

");

sub get_version_from_el {
    open (EL, "lisp/epl.el") || die ("Can't read lisp/epl.el: $!");
    while (<EL>) {
	return $1 if /^\(defconst epl-version "(.*?)"/;
    }
    die ("epl-version not defined in lisp/epl.el");
}

sub MY::postamble {
    return q(
# Building XEmacs packages.  This has to happen in the xemacs-packages
# CVS tree.  See xemacs/Makefile.

IN_XEMACS_TREE = test -f ../../XEmacs.rules

srckit: dist

binkit:
	$(IN_XEMACS_TREE)
	$(MAKE) -f xemacs/Makefile $@

clean::
	if $(IN_XEMACS_TREE); then $(MAKE) -f xemacs/Makefile $@; fi

realclean::
	if $(IN_XEMACS_TREE); then $(MAKE) -f xemacs/Makefile distclean; fi
)
}
