use ExtUtils::MakeMaker;

WriteMakefile (
	       'NAME'  => 'Emacs::EPL',
	       'VERSION_FROM' => 'lib/Emacs/EPL.pm',
	      );

warn ("
***NOTE

To run `make test', you need GNU Emacs 21.  Copy the files in the elisp
directory to where Emacs can find them, and set the EMACS environment
variable to Emacs' invocation name if other than `emacs'.

The test script `40_error.t' is known to fail.

");
