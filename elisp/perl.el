;; perl.el -- interactive functions for Perl embedded in Emacs
;; Copyright (C) 1998-2001 by John Tobey.  All rights reserved.

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


(require 'perl-core)

(defvar perl-interpreter-args nil
  "Default command line arguments for initializing a Perl interpreter.
This should be a list of strings, not including the program invocation
name or script name.  A \"-e\" script will be appended to the list to
ensure that the interpreter does not attempt to read its script from
the standard input.

See `make-perl-interpreter'.")

;; This gets called from C the first time a Perl function is called.
(defun make-perl-interpreter (&rest cmdline)
  "Create and return a new Perl interpreter object.
If arguments are given, they will be parsed as the interpreter's command
line, with the first argument used as the invocation name.  Otherwise, if
`perl-interpreter-args' is non-nil, the command line will be the pmacs
invocation name, followed by the list values in `perl-interpreter-args',
followed by \"-e0\".  Otherwise, the argument list defaults to
'(\"-MEmacs\" \"-MEmacs::Lisp\"), which causes the Emacs module to be loaded
and the Lisp function namespace to be exported to Perl.

It is important to include a script name or \"-e\" option when running
interactively, because otherwise Perl tries to read its script from the
standard input.  XXX It is a good idea to include \"-MEmacs\", as this makes
some special Perl variables and functions behave in a manner appropriate
for the Emacs environment."
  (let ((interp
	 (if (fboundp 'perl-interpreter-new)  ; EPL
	     (apply 'perl-interpreter-new
		    (or cmdline
			(cons perl-interpreter-program
			      (append (or perl-interpreter-args
					  '("-MEmacs" "-MEmacs::Lisp"))
				      '("-MEmacs::PlServer"
					"-weEmacs::PlServer::loop")))))
	   (apply 'primitive-make-perl  ; Perlmacs
		  (or cmdline
		      (cons (car command-line-args)  ; propagate argv[0]
			    (append (or perl-interpreter-args
					'("-MEmacs" "-MEmacs::Lisp"))
				    '("-we0"))))))))
    ;; Alas, this hook isn't called in batch mode.
    ;; XXX It would be nice to keep a weak hash table of interpreters
    ;; and destruct any remaining ones at the end.
    (add-hook 'kill-emacs-hook
	      `(lambda () (perl-destruct ,interp)))
    interp))

(defun perl-eval-expression (expression &optional prefix)
  "Evaluate EXPRESSION as Perl code and print its value in the minibuffer.
With prefix arg, evaluate in list context."
  (interactive (list (read-from-minibuffer "Eval Perl: ")
		     current-prefix-arg))
  (message (prin1-to-string
	    (perl-eval expression
		       (if prefix 'list-context 'scalar-context)))))

(defun perl-eval-region (start end)
  "Execute the region as Perl code."
  (interactive "r")
  (perl-eval (buffer-substring start end)))

(defun perl-eval-buffer ()
  "Execute the current buffer as Perl code."
  (interactive)
  (perl-eval (buffer-string)))

(defun perl-load-file (filename)
  "Apply Perl's `require' operator to FILENAME."
  (interactive "FLoad Perl file: ")
  (perl-eval-and-call "sub {require $_[0]}" (expand-file-name filename)))

(defvar perl-map nil "Keymap for Perl-specific operations.")
(when (not perl-map)
  (setq perl-map (make-sparse-keymap))
  (define-key perl-map "e"    'perl-eval-expression)
  (define-key perl-map "r"    'perl-eval-region)
  (if (not (lookup-key global-map "\C-xp"))
      (define-key global-map "\C-xp" perl-map)))


(provide 'perl)
