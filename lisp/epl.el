;; epl.el -- Perl interpreter in a separate process
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


(defconst epl-version "0.5" "Version numbers of EPL.")

;; Gawd this would be so much easier in Perl.  :-)
(defconst epl-major-version
  (progn (string-match "^[0-9]+" epl-version)
	 (string-to-int (match-string 0 epl-version)))
  "Major version number of this version of EPL.")

(defconst epl-minor-version
  (progn (string-match "^[0-9]+\\.\\([0-9]+\\)" epl-version)
	 (string-to-int (match-string 1 epl-version)))
  "Minor version number of this version of EPL.")

(defvar epl-debug nil
  "If true, log messages in buffers \"epl-debug\" and \"perl\".
If `stderr', send them to the standard error stream instead.")

(defun epl-do-debug (object)
  (if (eq epl-debug 'stderr)
      (prin1 object 'external-debugging-output)
    (with-current-buffer (get-buffer-create "epl-debug")
      (if (stringp object)
	  (insert object)
	(prin1 object))
      (sit-for 0))))

(defsubst epl-debug (object)
  (if epl-debug (epl-do-debug object)))

(require 'epl-compat)

(put 'perl-error 'error-conditions '(perl-error error))
(put 'perl-error 'error-message "Perl error")

(defvar perl-interpreter nil
  "The current Perl interpreter object.
Functions like `perl-eval' and `perl-call' act implicitly on this value
and initialize it by starting Perl if it is nil.  To use a private
interpreter instance in Lisp code, set it locally with `let'.")

(defvar perl-interpreter-program "perl"
  "Default program name for external Perl interpreters.")

(defvar perl-interpreter-args '("-MEmacs" "-MEmacs::Lisp")
  "Default command line arguments for initializing a Perl interpreter.
This should be a list of strings, not including the program name or
script name.

See `make-perl-interpreter'.")

(defvar epl-interp nil
  "Copy of `perl-interpreter' used internally.  Don't alter this.")

(defvar epl-interp-map (make-hash-table :test 'eq :size 5)
  "Hash table mapping process object to Perl interpreter object.")

(defvar epl-exiting nil
  "True if trying to exit Emacs.")

(defun epl-kill-emacs-hook ()
  "Tell all Perl subprocesses to exit."
  (let ((epl-exiting t))
    (maphash (lambda (proc interp)
	       (if (processp proc)
		   (perl-destruct interp)))
	     epl-interp-map)))

(add-hook 'kill-emacs-hook 'epl-kill-emacs-hook)

(defvar epl-post-gc-flag nil
  "Set by epl-post-gc-hook, reset by epl-gc.")

(defun epl-post-gc-hook ()
  (setq epl-post-gc-flag t))

(defun epl-gc ()
  (maphash epl-interp-map
	   (error))
  (setq epl-post-gc-flag nil))

;; epl-interp type:
;; IN          == input stream
;; OUT         == output stream, a process object
;; BUFFER      == buffer for process output  XXX memory waste?
;; GCPRO       == hash table mapping handle to gc-protected Lisp object
;; NEXT-HANDLE == handle of next Lisp object to be wrapped
;; REFS        == weak hash table mapping handle to perl-value object
;; NREFS       == last count of REFS
;; STATUS      == nil, ready, destroyed, exit, signal
;; DEPTH       == how many requests we are currently handling

;; Let's see how long before I break down and go get defstruct...

(defun epl-interp-p (object)
  (and (vectorp object)
       (= (length object) 10)
       (eq (aref object 0) 'epl-interp-tag)))

(defmacro epl-interp-in           (interp)  (list 'aref interp 1))
(defmacro epl-interp-out          (interp)  (list 'aref interp 2))
(defmacro epl-interp-buffer       (interp)  (list 'aref interp 3))
(defmacro epl-interp-gcpro        (interp)  (list 'aref interp 4))
(defmacro epl-interp-next-handle  (interp)  (list 'aref interp 5))
(defmacro epl-interp-refs         (interp)  (list 'aref interp 6))
(defmacro epl-interp-nrefs        (interp)  (list 'aref interp 7))
(defmacro epl-interp-status       (interp)  (list 'aref interp 8))
(defmacro epl-interp-depth        (interp)  (list 'aref interp 9))

(defmacro epl-interp-set-in           (interp x)  (list 'aset interp 1 x))
(defmacro epl-interp-set-out          (interp x)  (list 'aset interp 2 x))
(defmacro epl-interp-set-buffer       (interp x)  (list 'aset interp 3 x))
(defmacro epl-interp-set-gcpro        (interp x)  (list 'aset interp 4 x))
(defmacro epl-interp-set-next-handle  (interp x)  (list 'aset interp 5 x))
(defmacro epl-interp-set-refs         (interp x)  (list 'aset interp 6 x))
(defmacro epl-interp-set-nrefs        (interp x)  (list 'aset interp 7 x))
(defmacro epl-interp-set-status       (interp x)  (list 'aset interp 8 x))
(defmacro epl-interp-set-depth        (interp x)  (list 'aset interp 9 x))

(defun make-epl-interp (&rest named-args)
  (let ((interp (make-vector 10 nil))
	name value)
    (aset interp 0 'epl-interp-tag)
    (while named-args
      (setq name (car named-args)
	    value (car-safe (cdr named-args))
	    named-args (cdr-safe (cdr named-args)))
      (cond ((eq name :in)          (epl-interp-set-in interp value))
	    ((eq name :out)         (epl-interp-set-out interp value))
	    ((eq name :depth)       (epl-interp-set-depth interp value))
	    (t (signal 'error "Invalid argument list" name))))
    (epl-interp-set-gcpro interp (make-hash-table :test 'eq :size 20))
    (epl-interp-set-buffer interp (generate-new-buffer
				   (if (processp (epl-interp-out interp))
				       (process-name (epl-interp-out interp))
				     "perl")))
    (epl-interp-set-next-handle interp 1)
    (epl-interp-set-refs interp (epl-make-refs-hash-table))
    (epl-interp-set-nrefs interp 0)
    interp))

(defun perl-interpreter-new (&rest cmdline)
  "Used internally by `make-perl-interpreter'.
Create and return a new Perl interpreter object."
  (let* ((process-connection-type nil)  ; Use a pipe.
	 (out (apply 'start-process "perl" nil
		     (or cmdline
			 (append
			  (list perl-interpreter-program)
			  (mapcar (lambda (dir) (concat "-I" dir))
				  (epl-perllib))
			  (list (format "-MEmacs::EPL=%d.%03d,:server"
					epl-major-version epl-minor-version))
			  perl-interpreter-args
			  '("-eEmacs::EPL::loop")))))
	 (epl-interp (make-epl-interp :in nil :out out :depth 0)))
    (epl-interp-set-in epl-interp
		       `(lambda (&optional ch)
			  (epl-read-char ,epl-interp ch)))
    (process-kill-without-query out)
    (set-process-filter out 'epl-filter)
    (set-process-sentinel out 'epl-sentinel)
    (epl-puthash out epl-interp epl-interp-map)
    ;; Wait for the handshake message.
    (condition-case err
	(epl-loop)
      (error (perl-destruct epl-interp)
	     (signal (car err) (cdr err))))
    (epl-interp-set-status epl-interp 'ready)
    epl-interp))

(defun perl-interpreter-p (object)
  "Return t if OBJECT is a Perl interpreter"
  (epl-interp-p object))

(defun perl-destruct (&optional interpreter)
  "Attempt to shut down the specified Perl interpreter.
If no arg is given, shut down the current Perl interpreter."
  (or interpreter (setq interpreter perl-interpreter))
  (if (epl-interp-p interpreter)
      (let ((status (epl-interp-status interpreter)))
	(if (and (eq status 'running)
		 (not epl-exiting))
	    (error
	     (format "Attempt to kill Perl from within Perl%s"
		     (if (processp (epl-interp-out interpreter))
			 "; use `M-x top-level RET' first"
		       ""))))
	(set-process-sentinel (epl-interp-out interpreter) nil)
	(if (eq status 'ready)
	    (condition-case nil
		(let ((epl-interp interpreter))
		  (epl-send-message "&cb_exit()"))
	      (error nil)))
	(if (not (eq status 'destroyed))
	    (condition-case nil
		(progn
		  (remhash (epl-interp-out interpreter)
			   epl-interp-map)
		  (kill-buffer (epl-interp-buffer interpreter))
		  (delete-process (epl-interp-out interpreter)))
	      (error nil)))
	(epl-interp-set-status interpreter 'destroyed)))
  (if (eq perl-interpreter interpreter)
      (setq perl-interpreter nil)))

(defun epl-check ()
  (if (perl-interpreter-p perl-interpreter)
      perl-interpreter
    (signal 'wrong-type-argument 'perl-interpreter-p perl-interpreter)))

(defun epl-init ()
  (if perl-interpreter
      (epl-check)
    (setq perl-interpreter (perl-interpreter-new))))

;; Return a search list of possible Perl module directories.
(defun epl-perllib ()
  (let (dirs)
    (mapcar (lambda (elt)
	      (when (stringp elt)
		(setq elt (concat elt "perllib"))
		(if (file-directory-p elt)
		    (setq dirs (cons elt dirs)))))
	    (if (boundp 'data-directory-list)
		data-directory-list
	      (if (boundp 'data-directory)
		  (list data-directory))))
    (nreverse dirs)))

(defun epl-filter (proc string)
  (save-excursion
    (let ((interp (gethash proc epl-interp-map)))
      (when (epl-interp-p interp)
	(set-buffer (epl-interp-buffer interp))
	;; Insert the text, advancing the process marker.
	(goto-char (point-max))
	(insert string)
	(set-marker (process-mark proc) (point))))))

(defun epl-sentinel (proc string)
  (let ((interp (gethash proc epl-interp-map nil)))
    (if interp
	(epl-interp-set-status interp (process-status proc))
	(perl-destruct interp)
	(if (eq interp epl-interp)
	    ;; XXX newline
	    (error "Perl subprocess died unexpectedly (%s)" string)
	  (message "Perl subprocess died unexpectedly (%s)" string)))))

;; perl-value type:
;; INTERPRETER  == owner perl-interpreter
;; HANDLE       == integer id, unique per interpreter

(defun perl-value-p (object)
  "Return t if OBJECT is a Perl scalar value."
  (and (vectorp object)
       (= (length object) 3)
       (eq (aref object 0) 'perl-value-tag)))

(defmacro perl-value-interpreter (value) (list 'aref value 1))
(defmacro perl-value-handle      (value) (list 'aref value 2))

(defun epl-read-char (interp ch)
  (let* ((out (epl-interp-out interp)))
    (with-current-buffer (epl-interp-buffer interp)
      (if ch
	  (progn
	    (unless (eq ch (preceding-char))
	      (insert-char ch))
	    (backward-char))
	(if (eobp)
	    (accept-process-output out))
	(forward-char)
	(char-before)))))

;;;###autoload
(defun perl-eval (string &optional context)
  "Evaluate STRING as Perl code, returning the value of the last expression.
If specified, CONTEXT must be either `scalar-context', `list-context', or
`void-context'.  By default, a scalar context is supplied."
  (epl-eval (epl-init) nil context
	    "do { package main;\n" string " }"))

;;;###autoload
(defun perl-eval-raw (string &optional context)
  "Evaluate STRING as Perl code, returning its value as Perl data.
This function is exactly the same as `perl-eval' except in that it does not
convert its result to Lisp."
  (epl-eval (epl-init) t context
	    "do { package main;\n" string " }"))

;; XXX "context-and-args" appears in describe-function.
;;;###autoload
(defun perl-call (sub &rest context-and-args)
  "Call a Perl sub or coderef with arguments.

SUB may be a string containing a sub name, a Perl coderef, or a Lisp
function.  The behavior when SUB is a Lisp function is the same as
that of `funcall'.

The second argument specifies the calling context if it is one of the
symbols `scalar-context', `list-context', or `void-context'.  If the
second argument to `perl-call' is none of these, a scalar context is
used, and the second argument, if present, is prepended to the list of
remaining args.  The remaining args are converted to Perl and passed
to the sub or coderef.


(perl-call SUB &optional CONTEXT &rest ARGS)"
  (epl-subcall nil sub context-and-args))

;; XXX "context-and-args" appears in describe-function.
;;;###autoload
(defun perl-call-raw (sub &rest context-and-args)
  "Call a Perl sub or coderef and return its result as Perl data.
This function is exactly the same as `perl-call' except in that it does not
convert its result to Lisp.


(perl-call-raw SUB &optional CONTEXT &rest ARGS)"
  (epl-subcall t sub context-and-args))

;; XXX "context-and-args" appears in describe-function.
;;;###autoload
(defun perl-eval-and-call (string &rest context-and-args)
  "Same as `perl-call' but evaluate the first arg to get the coderef.

The first argument should be a string of Perl code which evaluates to a
sub name or coderef.  The remaining arguments are treated the same as in
`perl-call'.


(perl-eval-and-call STRING &optional CONTEXT &rest ARGS)"
  ;; XXX can be more efficient
  (apply 'perl-call (perl-eval string) context-and-args))

(defun epl-subcall (rawp sub args)
  (let* ((perl-interpreter (if (perl-value-p sub)
			       (perl-value-interpreter sub)
			     (epl-init)))
	 (epl-interp perl-interpreter)
	 context quoted-p)
    ;; Accommodate the calling signature of perl-call and perl-call-raw
    ;; (first element of args is actually context if it is recognizable
    ;; as such, else it is really the first arg).
    (if args
	(progn
	  (setq context (car args)
		args (cdr args))
	  (cond ((eq context 'scalar-context) nil)
		((eq context 'list-context) nil)
		((eq context 'void-context) nil)
		(t (progn (setq args (cons context args))
			  (setq context 'scalar-context))))))
    (if (functionp sub)
	(apply 'funcall sub args)
      (if (stringp sub)
	  (let* ((simple-p (string-match "\\`[a-zA-Z_][a-zA-Z0-9_]*\\'" sub))
		 (qualified-p (and (not simple-p)
				   (string-match "'\\|::" sub))))
	    (if (not simple-p)
		;; See if we have to use &{"SUB"}() instead of &SUB().
		(let ((split (split-string sub "'\\|::")))
		  (and split (= (length (car split)) 0)
		       (setq split (cdr split)))
		  (mapcar
		   (lambda (name)
		     (or (string-match "\\`[a-zA-Z_][a-zA-Z0-9_]*\\'" name)
			 (setq quoted-p t)))
		   split)))
	    ;; Make an unqualified name refer to package main, even though
	    ;; the eval will be in a private package.
	    (if (not qualified-p)
		(setq sub (concat "::" sub))))
	;; If not a string, quoted-p is t.  (Need {} after &)
	(setq quoted-p t))
      (epl-eval
       perl-interpreter rawp context
       (if quoted-p
	   (list "&{" (epl-recursive-serialize sub) "}(")
	 (list "&" sub "("))
       (and args
	    (list "@{" (epl-serialize args) "}")) ")"))))

(defun epl-eval (interp rawp context &rest text)
  (let ((epl-interp interp)
	text-begin text-end)
    (cond ((eq context 'list-context)
	   (setq text-begin "[do {" text-end "}]"))
	  ((eq context 'void-context)
	   (setq text-begin "do { " text-end "; undef }"))
	  ((or (eq context 'scalar-context)
	       (null context)) nil)
	  (t (error "Unknown context for perl-eval" context)))
    (if rawp
	(progn
	  (epl-cb-handle-to-perl-value
	   (epl-send-and-receive "&cb_ref_to_handle(\\("
				 text-begin text text-end
				 "))")))
      (epl-send-and-receive text-begin text text-end))))

(defun epl-send-string (out string)
  (epl-debug string)
  (if (processp out)
      (process-send-string out string)
    (princ string out)))

;; Send all the strings in a structure of lists and strings to a process.
;; Implement buffering to avoid a write(2) call per string.  *sigh*
;; XXX untested.
(defconst epl-big-string-size 8192)
(defun epl-send-strings (out strings stack)
  (if (stringp strings)
      (progn
	(let ((olen (cdr stack))
	      (nlen (epl-string-bytes strings)))
	  (if (< (+ olen nlen) epl-big-string-size)
	      (progn
		(setcar stack (cons strings (car stack)))
		(setcdr stack (+ olen nlen)))
	    (epl-flush out stack)
	    (if (< nlen epl-big-string-size)
		(progn
		  (setcar stack (cons strings nil))
		  (setcdr stack nlen))
	      (epl-send-string out strings)
	      (setcar stack nil)
	      (setcdr stack 0)))))
    (while strings
      (epl-send-strings out (car strings) stack)
      (setq strings (cdr strings)))))

(defun epl-flush (out stack)
  (if (car stack)
      (epl-send-string out (apply 'concat (nreverse (car stack))))))

;; Return the total byte length of all strings in a structure of lists
;; and strings.
(defun epl-measure-strings (strings)
  (if (stringp strings)
      (epl-string-bytes strings)
    (apply '+ (mapcar 'epl-measure-strings strings))))

(defun epl-send-message (&rest text)
  (or (eq (epl-interp-status epl-interp) 'ready)
      (eq (epl-interp-status epl-interp) nil)
      (error "Perl has exited"))
  (epl-debug "\n>>> ")
  (let ((out (epl-interp-out epl-interp))
	(stack (cons nil 0)))
    (epl-send-strings out
		      (cons (format "%d\n"
				    (epl-measure-strings text))
			    text)
		      stack)
    (epl-flush out stack)))

(defun epl-send-and-receive (&rest text)
  (let ((inhibit-quit t))
    (apply 'epl-send-message text)
    (epl-loop)))

;; Answer requests until we get our reply or an error.
(defun epl-loop ()
  (let ((depth (epl-interp-depth epl-interp)))
    (unwind-protect
	(catch 'return
	  (epl-interp-set-depth epl-interp (1+ depth))
	  (while t
	    (let ((form (read (epl-interp-in epl-interp)))
		  reply done msg)
	      (epl-debug "\n<<< ")
	      (epl-debug form)
	      (unwind-protect
		  (setq reply
			(condition-case err
			    (catch 'epl-reply
			      (throw 'epl-reply
				     (cons 'return-to-perl (eval form))))
			  (error (cons 'raise err)))
			done t)
		(when (not done)
		  ;; Oops, exiting abnormally via `throw'.
		  (if (and (eq depth 0)
			   (not (processp (epl-interp-out epl-interp))))
		      ;; Protocol error if we don't trap this.
		      (throw 'return nil))
		  ;; All sorts of things can happen during this reentry.
		  (epl-send-and-receive "&cb_pop()")))
	      (setq msg (car reply) reply (cdr reply))
	      (cond ((eq msg 'return) (throw 'return reply))
		    ((eq msg 'pop)
		     (if (eq depth 0)
			 (throw 'return nil))
		     (epl-send-message "&cb_return()")
		     (throw 'epl-reply '(skip)))
		    ((eq msg 'skip) nil)
		    ((eq msg 'exit)
		     (when (eq depth 0)
		       (epl-interp-set-depth epl-interp 0)
		       (perl-destruct epl-interp)
		       (error "Exiting a calling Perl"))
		     (epl-send-and-receive "&cb_pop()")
		     (throw 'epl-reply '(exit)))
		    ((eq msg 'propagate) (signal (car reply) (cdr reply)))
		    ((eq msg 'raise)
		     (epl-send-message "&cb_raise("
				       (epl-serialize-exception reply)
				       ")"))
		    ((eq msg 'return-to-perl)
		     (epl-send-message "&cb_return(" reply ")"))
		    (t (error "huh? %s" msg))))
	    ;; Erase the message buffer if not debugging.
	    (or epl-debug
		(with-current-buffer (epl-interp-buffer epl-interp)
		  (erase-buffer)))))
      (epl-interp-set-depth epl-interp depth))))

;; "epl-cb-" functions are called by evalled messages.

(defun epl-cb-return (ret)
  (throw 'epl-reply (cons 'return ret)))

(defun epl-cb-handle-to-perl-value (handle)
  (let ((refs (epl-interp-refs epl-interp)))
    (or (gethash handle refs)
	;; XXX They should document the return value of puthash.
	(let (obj)
	  (prog1
	      (setq obj (vector 'perl-value-tag epl-interp handle))
	    (epl-puthash handle obj refs)
	    (epl-interp-set-nrefs
	     epl-interp
	     (1+ (epl-interp-nrefs epl-interp)))
	    (epl-update-nrefs-maybe-gc refs))))))

(defun epl-update-nrefs-maybe-gc (refs)
  (let ((old-nrefs (epl-interp-nrefs epl-interp))
	(new-nrefs (hash-table-count refs)))
    (when (> old-nrefs new-nrefs)
      (let (elts)
	(maphash (lambda (handle value)
		   (setq elts (cons (format "%d," handle) elts)))
		 refs)
	(epl-send-and-receive "&cb_free_refs_except(" elts ")"))
      (epl-interp-set-nrefs epl-interp new-nrefs)
      (- old-nrefs new-nrefs))))

;; Send an UNREF type message promising never again to refer to this handle.
(defun epl-free-handle (handle)
  (when (not (eq (gethash handle (epl-interp-refs epl-interp) 'epl-nope)
		 'epl-nope))
    (epl-interp-set-nrefs epl-interp
			  (1- (epl-interp-nrefs epl-interp)))
    (remhash handle (epl-interp-refs epl-interp))
    (epl-send-and-receive (format "&cb_unref(%d)" handle))))

(defun perl-free-refs (&rest refs)
  "Release references to Perl data.
This happens automatically if Emacs supports weak hash tables, as XEmacs
21 and GNU Emacs 21 do."
  (while refs
    (let ((ref (car refs)))
      (if (perl-value-p ref)
	  (let ((epl-interp (perl-value-interpreter ref)))
	    (epl-free-handle (perl-value-handle ref)))))))

(defun perl-gc (&optional purge)
  "Release any Perl references that have been garbage-collected.
This happens automatically if Emacs supports weak hash tables, as XEmacs
21 and GNU Emacs 21 do.  See `perl-free-refs'.

If PURGE is true (interactively, with prefix arg), repeatedly call
`garbage-collect' and release Perl references until all reference chains
are freed."
  (interactive "P")
  (let* ((epl-interp perl-interpreter)
	 (refs (epl-interp-refs epl-interp)))
    (if purge
	(while (progn
		 (garbage-collect)
		 (epl-update-nrefs-maybe-gc refs)))
      (epl-update-nrefs-maybe-gc refs))))

(defun epl-cb-make-hash-table (&rest namevals)
  "Create a hash table and initialize it with alternating keys and values.
The new table uses `equal' as its test."
  (let ((h (make-hash-table :test 'equal)))
    (while namevals
      (epl-puthash (car namevals) (car (cdr namevals)) h)
      (setq namevals (cdr (cdr namevals))))
    h))

;; This function is a good place to set a breakpoint.
(defun epl-serialize-exception (err)
  ;; If the error originated from Perl, strip off our outer wrappings.
  (if (eq (car err) 'perl-error)
      (epl-serialize (car (cdr err)))
    ;; Supposing it is a Java error (for instance).  Let's say Java called
    ;; Perl, Perl called us, we called Java, and Java threw us err.
    ;; We would like to give it to Perl in a form that Perl will recognize
    ;; as Javonic and be able to unwrap for Java just as we unwrap Perlish
    ;; exceptions for Perl.
    ;; Answer: We'll cross that bridge when we come to it.
    ;; Assume err was signaled by Lisp.
    (list (epl-serialize (error-message-string err)) ","
	  (epl-serialize err))))

(defun epl-cb-call (args)
  (epl-serialize (apply 'funcall args)))

(defun epl-cb-call-void (args)
  (apply 'funcall args)
  nil)

(defun epl-cb-call-raw (args)
  (epl-serialize-opaque (apply 'funcall args)))

;; Passed to epl-cb-call and epl-cb-call-raw to implement wrap and
;; unwrap operations.
(defun epl-echo-one-arg (object)
  object)

(defun epl-cb-raise (err)
  (throw 'epl-reply (list 'propagate 'perl-error err)))

(defun epl-cb-propagate (err)
  (throw 'epl-reply (cons 'propagate err)))

(defun epl-cb-pop (unused)
  (throw 'epl-reply '(pop)))

;; Serialization state.
(defun epl-ss-new () (vector (make-hash-table :test 'eq) nil t))
(defmacro epl-ss-seen        (ss) (list 'aref ss 0))
(defmacro epl-ss-fixup       (ss) (list 'aref ss 1))
(defmacro epl-ss-pos         (ss) (list 'aref ss 2))
(defmacro epl-ss-set-fixup   (ss new) (list 'aset ss 1 new))
(defmacro epl-ss-set-pos     (ss new) (list 'aset ss 2 new))

(defun epl-serialize (value)
  (epl-recursive-serialize value (epl-ss-new)))

;; perl-ref: [perl-ref-tag VALUE]
;; A SCALAR ref.
(defun epl-cb-ref (value) (vector 'perl-ref-tag value))
(defun perl-ref-p (object)
  (and (vectorp object)
       (= (length object) 2)
       (eq (aref object 0) 'perl-ref-tag)))
(defmacro perl-ref-value (ref) (list 'aref ref 1))
(defmacro perl-ref-set-value (ref value) (list 'aset ref 1 value))

(defun epl-cb-coderef (handle)
  `(lambda (&rest perl-coderef-tag)
     (apply 'perl-call
	    ,(epl-cb-handle-to-perl-value handle)
	    perl-coderef-tag)))

;; XXX Need a perl-coderef-p function for users.
;; Should it return t for non-lambdaized objects?
(defun epl-coderef-p (object)
  (and (functionp object)
       (eq (car-safe (cdr-safe (car-safe (cdr-safe object))))
	   'perl-coderef-tag)))

(defun epl-coderef-value (object)  (nth 2 (nth 2 object)))

(defun epl-fixup (from to)
  (error "epl.el does not convert circular data yet"))

(defun epl-recursive-serialize (value &optional state)
  (cond ((stringp value) (epl-serialize-string value))
	((numberp value) (number-to-string value))
	((null value)    "undef")
	((symbolp value) (epl-serialize-symbol value))
	((and (or (perl-value-p value)
		  (and (epl-coderef-p value)
		       (setq value (epl-coderef-value value))))
	      (eq (perl-value-interpreter value) epl-interp))
	 (format "&cb_handle_to_ref(%d)" (perl-value-handle value)))
	(t
	 (or state (error "No state object while serializing a structure"))
	 (let ((seen (gethash value (epl-ss-seen state))))
	   (if seen
	       (progn
		 (epl-ss-set-fixup
		  state
		  (cons (epl-fixup (epl-ss-pos state) seen)
			(epl-ss-fixup state)))
		 nil)
	     (epl-puthash value (epl-ss-pos state) (epl-ss-seen state))
	     (cond ((consp value)        (epl-serialize-cons value state))
		   ((perl-ref-p value)   (epl-serialize-ref value state))
		   ((vectorp value)      (epl-serialize-vector value state))
		   ((hash-table-p value) (epl-serialize-hash value state))
		   (t (epl-serialize-opaque value))))))))

;; Perform the Lisp equivalent of C<$string =~ s/([\\'])/\\$1/g; "'$string\'">.
(defun epl-serialize-string (string)
  (format "'%s'"
	  (if (string-match "['\\]" string)
	      (with-temp-buffer
		(insert string)
		(goto-char (point-min))
		(while (re-search-forward "['\\]" nil t)
		  (replace-match "\\\\\\&"))
		(buffer-string))
	    string)))

;; Perform the Lisp equivalent of tr/-_/_-/ and handle funny names specially.
;; XXX What if name contains :: or ' ?
(defun epl-serialize-symbol (sym)
  (let* ((name (copy-sequence (symbol-name sym)))
	 (pos (length name)))
    (while (> pos 0)
      (setq pos (1- pos))
      (let ((ch (aref name pos)))
	(cond ((= ch ?-) (aset name pos ?_))
	      ((= ch ?_) (aset name pos ?-)))))
    (if (string-match "\\`[a-zA-Z_][a-zA-Z0-9_]*\\'" name)
	(concat "\\*::" name)
      (format "\\*{%s}" (epl-serialize-string (concat "::" name))))))

(defun epl-serialize-ref (ref state)
  ;; XXX alter pos.
  (list "\\("
	(epl-recursive-serialize (perl-ref-value ref) state)
	")"))

(defun epl-serialize-vector (value state)
  (list "\\["
	(mapcar (lambda (elt)
		  ;; XXX alter pos here.
		  (list (epl-recursive-serialize elt state) ","))
		value)
	"]"))

(defun epl-serialize-hash (value state)
  (list "{ "
	(mapcar (lambda (elt)
		  ;; XXX alter pos here.
		  (list (epl-recursive-serialize (car elt) state)
			" => "
			;; XXX alter pos here.
			(epl-recursive-serialize (cdr elt) state)
			","))
		(maphash (lambda (k v) (cons k v)) value))
	" }"))

(defun epl-serialize-cons (value state)
  (let ((tail value) list)
    (while (consp tail)
      (setq list (cons ","
		       (cons (epl-recursive-serialize (car tail) state)
			     list))
	    tail (cdr tail)))
    (if (null tail)
	(list "[" (nreverse list) "]")
      (epl-serialize-pseudo-list (nreverse list) tail))))

(defun epl-serialize-pseudo-list (head tail)
  (if (null head)
      ;; XXX alter pos here?
      (epl-recursive-serialize tail)
    (list "&cb_cons(" (car head) ","
	  (epl-serialize-pseudo-list (cdr (cdr head)) tail) ")")))

(defun epl-serialize-opaque (value)
  (format "&cb_object(%d)"
	  (epl-object-to-handle value)))

(defun epl-object-to-handle (object)
  (let ((handle (epl-interp-next-handle epl-interp)))
    (epl-interp-set-next-handle epl-interp (1+ handle))
    (epl-puthash handle object (epl-interp-gcpro epl-interp))
    handle))

(defun epl-cb-unref (handles)
  (while handles
    (remhash (car handles) (epl-interp-gcpro epl-interp))
    (setq handles (cdr handles)))
  ;; XXX probably should do a perl-gc here
  )

(defun epl-cb-handle-to-object (handle)
  (gethash handle (epl-interp-gcpro epl-interp)))

(defun perl-to-lisp (object)
  "Return a deep copy of OBJECT replacing Perl data with Lisp equivalents.
Arrayrefs are converted to lists.  References to arrayrefs become vectors.
Coderefs become lambda expressions.

If the object is not Perl data, it is returned unchanged.  See `perl-value-p'."
  (if (perl-value-p object)
      (let ((epl-interp (perl-value-interpreter object)))
	(epl-send-and-receive (format "&cb_handle_to_ref(%d)"
				      (perl-value-handle object))))
    object))

;;;###autoload
(defun perl-wrap (object)
  "Return OBJECT as a Perl object of class Emacs::Lisp::Object."
  (let ((epl-interp (epl-init)))
    (epl-cb-handle-to-perl-value
     (epl-send-and-receive "&cb_ref_to_handle("
			   (epl-serialize-opaque object)
			   ")"))))


(provide 'perl-core)
(provide 'epl)

;; end of epl.el
