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


(defconst epl-version "0.1" "\
Version numbers of EPL.")

;; Gawd this would be so much easier in Perl.  :-)
(defconst epl-major-version
  (progn (string-match "^[0-9]+" epl-version)
	 (string-to-int (match-string 0 epl-version)))
  "Major version number of this version of EPL.")

(defconst epl-minor-version
  (progn (string-match "^[0-9]+\\.\\([0-9]+\\)" epl-version)
	 (string-to-int (match-string 1 epl-version)))
  "Minor version number of this version of EPL.")

;; Compatibility.  XXX Should we even be using string-bytes??
(or (fboundp 'string-bytes)
    (fset 'string-bytes 'length))

(defvar perl-interpreter nil
  "The current active Perl interpreter, or `nil' if none is active.")

(defvar epl-debug nil
  "If true, log messages in buffer \"epl-debug\".")
(defsubst epl-debug (object)
  (if epl-debug
      (with-current-buffer (get-buffer-create "epl-debug")
	(if (stringp object)
	    (insert object)
	  (prin1 object))
	(sit-for 0))))

(defvar perl-interpreter-program "perl"
  "Default program name for external Perl interpreters.")

(defvar epl-interp nil
  "Copy of `perl-interpreter' used internally.  Don't alter this.")

;; perl-interpreter type:  [perl-interpreter-tag PROC BUF GCPRO NEXT-HANDLE
;;  REFS NREFS]
;; PROC == process object
;; BUF == buffer for process output
;; GCPRO == hash table mapping handle to gc-protected object
;; NEXT-HANDLE == handle of next wrapped Lisp object
;; REFS == weak hash table mapping Perl handle to Lisp perl-value object
;; NREFS == last count of REFS

(defun perl-interpreter-p (object)
  "Return t if OBJECT is a Perl interpreter"
  (and (vectorp object)
       (= (length object) 7)
       (eq (aref object 0) 'perl-interpreter-tag)))

(defmacro perl-interpreter-process      (interp)  (list 'aref interp 1))
(defmacro perl-interpreter-buffer       (interp)  (list 'aref interp 2))
(defmacro perl-interpreter-gcpro        (interp)  (list 'aref interp 3))
(defmacro perl-interpreter-next-handle  (interp)  (list 'aref interp 4))
(defmacro perl-interpreter-refs         (interp)  (list 'aref interp 5))
(defmacro perl-interpreter-nrefs        (interp)  (list 'aref interp 6))

(defmacro perl-interpreter-set-next-handle (interp handle)
  (list 'aset interp 4 handle))
(defmacro perl-interpreter-set-nrefs (interp n)  (list 'aset interp 6 n))

(defun perl-interpreter-new (&rest cmdline)
  "Used internally by `make-perl-interpreter'.
Create and return a new Perl interpreter object."
  (let* ((process-connection-type nil)  ; Use a pipe.
	 (pp (apply 'start-process "perl" nil
		    (or cmdline '(perl-interpreter-program
				  "-MEmacs::EPL"
				  "-weEmacs::EPL::loop"))))
	 (pb (generate-new-buffer (process-name pp)))
	 (interp (vector 'perl-interpreter-tag
			 pp
			 pb
			 (make-hash-table :test 'eq)
			 1
			 (make-hash-table :test 'eq :weakness 'value)
			 0)))
    (process-kill-without-query pp)
    (set-process-filter pp
			`(lambda (proc string)
			   (epl-filter ,interp string)))
    interp))

(defun epl-check ()
  (if (perl-interpreter-p perl-interpreter)
      perl-interpreter
    (signal 'wrong-type-argument 'perl-interpreter-p perl-interpreter)))

(defun epl-init ()
  (if perl-interpreter
      (epl-check)
    (setq perl-interpreter
	  (if (fboundp 'make-perl-interpreter)
	      (make-perl-interpreter)
	    (perl-interpreter-new)))))

(defun epl-filter (interp string)
  (save-excursion
    (let ((pm (process-mark (perl-interpreter-process interp))))
      (set-buffer (perl-interpreter-buffer interp))
      ;; Insert the text, advancing the process marker.
      (goto-char (point-max))
      (insert string)
      (epl-debug "\n<<< ")
      (epl-debug string)
      (set-marker pm (point)))))

;; perl-value type: [perl-value-tag INTERPRETER HANDLE]

(defun perl-value-p (object)
  "Return t if OBJECT is a Perl scalar value."
  (and (vectorp object)
       (= (length object) 3)
       (eq (aref object 0) 'perl-value-tag)))

(defmacro perl-value-interpreter (value) (list 'aref value 1))
(defmacro perl-value-handle      (value) (list 'aref value 2))

(defun epl-read-char (interp ch)
  (let* ((pp (perl-interpreter-process interp)))
    (with-current-buffer (perl-interpreter-buffer interp)
      (if ch
	  (progn
	    (unless (eq ch (preceding-char))
	      (insert-char ch))
	    (backward-char))
	(if (eobp)
	    (accept-process-output pp))
	(forward-char)
	(char-before)))))

(defun perl-eval (string &optional context)
  "Evaluate STRING as Perl code, returning the value of the last expression.
If specified, CONTEXT must be either `scalar-context', `list-context', or
`void-context'.  By default, a scalar context is supplied."
  (epl-eval (epl-init) nil context
	    "do { package main;\n" string " }"))

(defun perl-eval-raw (string &optional context)
  "Evaluate STRING as Perl code, returning its value as Perl data.
This function is exactly the same as `perl-eval' except in that it does not
convert its result to Lisp."
  (epl-eval (epl-init) t context
	    "do { package main;\n" string " }"))

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
  (if (functionp sub)
      (apply 'funcall sub context-and-args)
    (epl-subcall nil sub context-and-args)))

(defun perl-call-raw (sub &rest context-and-args)
  "Call a Perl sub or coderef and return its result as Perl data.
This function is exactly the same as `perl-call' except in that it does not
convert its result to Lisp.


(perl-call-raw SUB &optional CONTEXT &rest ARGS)"
  (epl-subcall t sub context-and-args))

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
    (if (stringp sub)
	(let* ((simple-p (string-match "\\`[a-zA-Z_][a-zA-Z0-9_]*\\'" sub))
	       (qualified-p (and (not simple-p) (string-match "'\\|::" sub))))
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
	  (list "@{" (epl-serialize args) "}"))
     ")")))

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
	  (epl-send-message "ref_to_handle(\\("
			    text-begin text text-end
			    "))")
	  (epl-cb-handle-to-perl-value (epl-loop)))
      (epl-send-message text-begin text text-end)
      (epl-loop))))

(defun perl-destruct (&optional interpreter)
  "Attempt to shut down the specified Perl interpreter.
If no arg is given, shut down the current Perl interpreter."
  (or interpreter (setq interpreter perl-interpreter))
  ;; XXX Should make epl-interp stack-like and test them all.
  (if (eq interpreter epl-interp)
      (error "Attempt to kill Perl from within Perl; use `M-x top-level RET' first"))
  (when (perl-interpreter-p interpreter)
    (condition-case nil
	(let ((epl-interpreter interpreter))
	  (epl-send-message "exit;"))
      (error nil))
    (condition-case nil
	(delete-process (perl-interpreter-process interpreter))
      (error nil))
    (condition-case nil
	(kill-buffer (perl-interpreter-buffer interpreter))
      (error nil))
    (if (eq perl-interpreter interpreter)
	(setq perl-interpreter nil))))

(defsubst epl-send-string (proc string)
  (epl-debug string)
  (process-send-string proc string))

;; Send all the strings in a structure of lists and strings to a process.
;; Implement buffering to avoid a write(2) call per string.  *sigh*
(defconst epl-big-string-size 8192)
(defun epl-send-strings (proc strings stack)
  (if (stringp strings)
      (progn
	(let ((olen (cdr stack))
	      (nlen (string-bytes strings)))
	  (if (< (+ olen nlen) epl-big-string-size)
	      (progn
		(setcar stack (cons strings (car stack)))
		(setcdr stack (+ olen nlen)))
	    (if (car stack)
		(epl-send-string proc
				 (apply 'concat (nreverse (car stack)))))
	    (if (< nlen epl-big-string-size)
		(progn
		  (setcar stack (cons strings nil))
		  (setcdr stack nlen))
	      (epl-send-string proc to-send)
	      (setcar stack nil)
	      (setcdr stack 0)))))
    (while strings
      (epl-send-strings proc (car strings) stack)
      (setq strings (cdr strings)))))

;; Return the total byte length of all strings in a structure of lists
;; and strings.
(defun epl-measure-strings (strings)
  (if (stringp strings)
      (string-bytes strings)
    (apply '+ (mapcar 'epl-measure-strings strings))))

(defun epl-send-message (&rest text)
  (epl-debug "\n>>> ")
  (let ((proc (perl-interpreter-process epl-interp))
	(stack (cons nil 0)))
    (epl-send-strings proc
		      (cons (format "%d\n"
				    (epl-measure-strings text))
			    text)
		      stack)
    (if (car stack)
	(epl-send-string proc (apply 'concat (nreverse (car stack)))))))

(defun epl-loop ()
  ;; Answer requests until we get our reply or an error.
  ;; XXX Maybe STREAM should be a member of the interpreter instead of
  ;; consed each time.
  (let ((stream `(lambda (&optional ch)
		   (epl-read-char ,epl-interp ch))))
    (catch 'epl-return
      (while t
	(eval (read stream))))))

;; "epl-cb-" functions are called by evalled messages.

(defun epl-cb-return (ret)
  (throw 'epl-return ret))

(defun epl-cb-handle-to-perl-value (handle)
  (let ((refs (perl-interpreter-refs epl-interp)))
    (or (gethash handle refs)
	;; XXX They should document the return value of puthash.
	(let (obj)
	  (prog1
	      (setq obj (vector 'perl-value-tag epl-interp handle))
	    (puthash handle obj refs)
	    (perl-interpreter-set-nrefs
	     epl-interp
	     (1+ (perl-interpreter-nrefs epl-interp)))
	    (epl-update-nrefs-maybe-gc refs))))))

(defun epl-update-nrefs-maybe-gc (refs)
  (let ((old-nrefs (perl-interpreter-nrefs epl-interp))
	(new-nrefs (hash-table-count refs)))
    (when (> old-nrefs new-nrefs)
      (let (elts)
	(maphash (lambda (handle value)
		   (setq elts (cons (format "%d," handle) elts)))
		 refs)
	(epl-send-message "&cb_free_refs_except(" elts ")")
	(epl-loop))
      (perl-interpreter-set-nrefs epl-interp new-nrefs)
      (- old-nrefs new-nrefs))))

(defun perl-gc (&optional purge)
  "Release any Perl references that have been garbage-collected.
This function is called automatically if Emacs supports weak hash tables,
as Emacs 21 and XEmacs 20(?) and above do.

If PURGE is true (interactively, with prefix arg), repeatedly call
`garbage-collect' and release Perl references until all reference chains
are freed."
  (interactive "P")
  (let* ((epl-interp perl-interpreter)
	 (refs (perl-interpreter-refs epl-interp)))
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
      (puthash (car namevals) (car (cdr namevals) h))
      (setq namevals (cdr (cdr namevals))))
    h))

;; XXX Use unwind-protect in case of throw?
(defun epl-return (func)
  (condition-case err
      (epl-send-message "return " (funcall func))
    ;; XXX should preserve the exception's structure
    (error (epl-die err))))

;; This is function is separate for the purpose of setting breakpoints.
(defun epl-die (err)
  (epl-send-message "die ("
		    (epl-recursive-serialize
		     (error-message-string err))
		    ")"))

(defun epl-cb-funcall (args)
  (epl-return (lambda ()
		(epl-serialize (apply 'funcall args)))))

(defun epl-cb-funcall-raw (args)
  (epl-return (lambda ()
		(epl-serialize-opaque (apply 'funcall args)))))

(defun epl-cb-unwrap (object)
  (epl-return (lambda ()
		(epl-serialize object))))

(defun epl-cb-wrap (object)
  (epl-return (lambda ()
		(epl-serialize-opaque object))))

(defun epl-cb-error (err)
  (error err))

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
(defun epl-cb-ref-new (value) (vector 'perl-ref-tag value))
(defun perl-ref-p (object)
  (and (vectorp object)
       (= (length object) 2)
       (eq (aref object 0) 'perl-ref-tag)))
(defmacro perl-ref-value (ref) (list 'aref ref 1))
(defmacro perl-ref-set-value (ref value) (list 'aset ref 1 value))

(defun epl-cb-wrap-coderef (handle)
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
	   (cond (seen
		  (epl-ss-set-fixup
		   state
		   (cons (epl-fixup (epl-ss-pos state) seen)
			 (epl-ss-fixup state)))
		  nil)
		 ((listp value)        (epl-serialize-list value state))
		 ((perl-ref-p value)   (epl-serialize-ref value state))
		 ((vectorp value)      (epl-serialize-vector value state))
		 ((hash-table-p value) (epl-serialize-hash value state))
		 ((consp value)        (epl-serialize-cons value state))
		 (t (epl-serialize-opaque value)))))))

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

(defun epl-serialize-arg-list (value state)
  (nconc (mapcar (lambda (elt)
		   ;; XXX alter pos here.
		   (nconc (epl-recursive-serialize elt state)
			  ","))
		 value)))

(defun epl-serialize-list (value state)
  (puthash value (epl-ss-pos state) (epl-ss-seen state))
  (list "["
	(mapcar (lambda (elt)
		  ;; XXX alter pos here.
		  (list (epl-recursive-serialize elt state) ","))
		value)
	"]"))

(defun epl-serialize-ref (ref state)
  ;; XXX alter pos.
  (list "\\("
	(epl-recursive-serialize (perl-ref-value ref) state)
	")"))

(defun epl-serialize-vector (value state)
  ;; XXX alter pos.
  (list "\\" (epl-serialize-list value state)))

(defun epl-serialize-hash (value state)
  (puthash value (epl-ss-pos state) (epl-ss-seen state))
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
  (list "&cb_cons("
	;; XXX alter pos here.
	(epl-recursive-serialize (car value) state)
	","
	;; XXX alter pos here.
	(epl-recursive-serialize (cdr value) state)
	")"))

(defun epl-serialize-opaque (value)
  (format "&cb_object(%d)"
	  (epl-object-to-handle value)))

(defun epl-object-to-handle (object)
  (let ((handle (perl-interpreter-next-handle epl-interp)))
    (perl-interpreter-set-next-handle interp (1+ handle))
    (puthash handle object (perl-interpreter-gcpro epl-interp))
    handle))

(defun epl-cb-unref-object (handle)
  (remhash handle (perl-interpreter-gcpro epl-interp)))

(defun epl-cb-handle-to-object (handle)
  (gethash handle (perl-interpreter-gcpro epl-interp)))

(defun perl-to-lisp (object)
  "Return a deep copy of OBJECT replacing Perl data with Lisp equivalents.
Arrayrefs are converted to a lists.  References to arrayrefs become vectors.
Coderefs become lambda expressions.

If the object is not Perl data, it is returned unchanged."
  (if (perl-value-p object)
      (let ((epl-interp (perl-value-interpreter object)))
	(epl-send-message (format "&cb_handle_to_ref(%d)"
				  (perl-value-handle object)))
	(epl-loop))
    object))


(provide 'perl-core)
(provide 'epl)

;; end of epl.el
