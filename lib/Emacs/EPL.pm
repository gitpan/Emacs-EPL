# Emacs::EPL - used internally by Emacs Perl.

package Emacs::EPL;

# eval free of 'strict'
sub my_eval { return (eval (shift)); }

require 5.000;  # well, it's a fantasy of mine.

use strict;
no strict 'refs';
use vars ('$VERSION', '$emacs', '$exiting', '$debug_stderr');
use Carp;

#$debug_stderr = 1;

$VERSION = '0.006';

BEGIN {
    # Set inlinable constants based on feature tests.
    local ($@);
    if (eval { require B; }) {
	if (defined (&B::SVf_IOK)) {
	    B->import (qw( SVf_IOK SVf_NOK ));
	    eval ('sub HAVE_B () { 1 }');
	}
	else {
	    eval ('sub HAVE_B () { 1 }');
	    eval ('sub SVf_IOK () { 0x10000 }');
	    eval ('sub SVf_NOK () { 0x20000 }');
	}
    }
    else {
	eval ('sub HAVE_B () { 0 }');
	if ($@) { eval ('sub HAVE_B { 0 }'); }
	eval ('sub SVf_IOK; sub SVf_NOK;');
    }
    eval { require overload; };
    if ($@) { eval ('sub overload::StrVal { return "$_[0]"; }'); }
}

# Tell whether a scalar is "really" an int, float, or string.
# The Elisp Reference Manual says that integers are 28 bits.
sub guess_lisp_type {
    if (HAVE_B()) {
	my $fl = B::svref_2object (\$_[0]) ->FLAGS;
	if (($fl & SVf_IOK) != 0) {
	    return ((($_[0] + 0x8000000) & ~0xfffffff) ? 'float' : 'integer');
	}
	elsif (($fl & SVf_NOK) != 0) {
	    return ('float');
	}
	else {
	    return ('string');
	}
    }
    else {
	if ($_[0] =~ m/^-?\d+$/) {
	    return ((($_[0] + 0x8000000) & ~0xfffffff) ? 'float' : 'integer');
	}
	elsif ($_[0] =~ m/^-?\d+(?:\.\d+)(?:e-?\d+)$/) {
	    return ('float');
	}
	else {
	    return ('string');
	}
    }
}

# print_stuff (CALLBACK, VALUE)
sub print_stuff {
    my $callback = $_[0];

    if (tied ($_[1]) || ref ($_[1])) {
	print( "(epl-cb-$callback (let ((epl-x `");
	local $$emacs {'pos'} = "epl-x";
	local $$emacs {'fixup'} = '';
	local $$emacs {'seen'};
	print_recursive ($_[1]);
	print( "))$$emacs{'fixup'} epl-x))");
    }
    else {
	# Optimize obviously non-circular cases.  (for visual aesthetics)
	print( "(epl-cb-$callback ");
	print_recursive ($_[1]);
	print( ")");
    }
}

# Given a reference, return its package (or undef if non-blessed),
# representation type, and unique identifier.
sub get_ref_info {
    # This is copied from Data::Dumper.
    return (overload::StrVal ($_[0]) =~ /^(?:(.*)\=)?([^=]*)\(([^\(]*)\)$/);
}

sub get_ref_id { return ((&get_ref_info) [2]); }
sub get_ref_types { return ((&get_ref_info) [0, 1]); }

# print_recursive(VALUE)
sub print_recursive {
    # Avoid unnecessary FETCH if tied.
    # Avoid unnecessary string copy.
    my ($ref);

    if (tied ($_[0])) {
	# This theoretically supports typed scalars.
	if (tied ($_[0]) ->can ("PRINT_AS_LISP")) {
	    tied ($_[0]) ->PRINT_AS_LISP;
	    return;
	}
	my $value = $_[0];
	$ref = \$value;
    }
    else {
	$ref = \$_[0];
    }
    if (ref ($$ref)) {
	my ($value) = @_;
	my ($id, $pos);

	$id = get_ref_id ($$ref);
	$pos = $emacs->{'seen'}->{$id};
	if (defined ($pos)) {
	    $emacs->{'fixup'}
		.= fixup ($emacs->{'pos'}, $pos);
	    print( "nil");
	}
	else {
	    $emacs->{'seen'}->{$id} = $emacs->{'pos'};
	    # This is like C<$$ref->epl_print_as_lisp($emacs)>
	    # but accepts unblessed $$ref.
	    & { ref ($$ref) ->can ('epl_print_as_lisp') } ($$ref);
	}
    }
    elsif (defined ($$ref)) {
	my $type = guess_lisp_type ($$ref);
	if ($type eq 'integer') {
	    print( 0 + $$ref);
	}
	elsif ($type eq 'float') {
	    my $value = 0 + $$ref;
	    $value .= ".0" if index ($value, '.') == -1;
	    print( $value);
	}
	else {  # string
	    if ($$emacs {'pid'}) {
		# XXX Make newlines \n because Emacs in -batch mode
		# can't handle newlines.
		if ($$ref =~ m/[\\\"\n]/) {
		    # About to modify, so copy if we have not done so yet.
		    # XXX Convert everything to syswrite() to avoid copy?
		    if (! tied ($_[0])) {
			my $value = $$ref;
			$ref = \$value;
		    }
		    $$ref =~ s/([\\\"])/\\$1/g;
		    $$ref =~ s/\n/\\n/g;
		}
	    }
	    else {
		if ($$ref =~ m/[\\\"]/) {
		    # About to modify, so copy if we have not done so yet.
		    # XXX Convert everything to syswrite() to avoid copy?
		    if (! tied ($_[0])) {
			my $value = $$ref;
			$ref = \$value;
		    }
		    $$ref =~ s/([\\\"])/\\$1/g;
		}
	    }
	    print( '"', $$ref, '"');
	}
    }
    else {
	print( "nil");
    }
}

sub print_blessed_ref {
    my ($value, $package, $meth) = @_;

    $package =~ s/\\/\\\\/g;
    $package =~ s/\"/\\\"/g;
    print( "(perl-blessed \"$package\" . ");
    local $emacs->{'pos'} = "(cdr (cdr $$emacs{'pos'}))";
    &$meth;
    print( ")");
}

sub UNIVERSAL::epl_print_as_lisp {
    my ($value) = @_;
    my ($package, $realtype, $meth);

    ($package, $realtype) = get_ref_types ($value);
    $package = $realtype if ! defined ($package);
    $meth = $package->can ('epl_print_as_lisp');

    if ($meth == \&UNIVERSAL::epl_print_as_lisp) {
	&print_opaque;
    }
    else {
	print_blessed_ref ($value, $package, $meth);
    }
}

sub Emacs::Lisp::Object::epl_print_as_lisp {
    my ($value) = @_;
    my ($e, $handle) = @$value;

    if ($$e {'id'} == $$emacs {'id'}) {
	delete ($emacs->{'seen'}->{ &get_ref_id });
	print( ",(epl-cb-handle-to-lisp $handle)");
    }
    else {
	print_blessed_ref ($value, ref ($value), \&ARRAY::epl_print_as_lisp);
    }
}

sub SCALAR::epl_print_as_lisp { &REF::epl_print_as_lisp }

sub REF::epl_print_as_lisp {
    my ($value) = @_;
    my ($class);

    $value = $$value;
    $class = ref ($value);
    if ($class eq 'ARRAY') {
	# ref-to-ref-to-array is a Lisp vector.
	my $opos = $emacs->{'pos'};
	local ($emacs->{'pos'});
	print( "[");
	for (my $i = 0; $i <= $#$value; $i++) {
	    $emacs->{'pos'} = "(aref $opos $i)";
	    print( " ") if $i > 0;
	    print_recursive ($$value [$i]);
	}
	print( "]");
    }
    else {
	print( ",(make-perl-ref `");
	local $emacs->{'pos'}
	    = "(perl-ref $$emacs{'pos'})";
	print_recursive ($value);
	print( ")");
    }
}

sub ARRAY::epl_print_as_lisp {
    my ($value) = @_;
    my ($opos);

    $opos = $emacs->{'pos'};
    local ($emacs->{'pos'});
    print( "(");
    for (my $i = 0; $i <= $#$value; $i++) {
	$emacs->{'pos'} = "(nth $i $opos)";
	print( " ") if $i > 0;
	print_recursive ($$value [$i]);
    }
    print( ")");
}

sub HASH::epl_print_as_lisp {
    my ($value) = @_;
    my ($opos);

    $opos = $emacs->{'pos'};
    local ($emacs->{'pos'});
    # Elisp lacks a read syntax for hash tables.
    print( ",(epl-cb-make-hash-table");
    while (my ($k, $v) = each (%$value)) {
	# XXX Force key to be a string or symbol - avoids issues.
	my ($name) = get_globref_name ($k);
	if (defined ($name)) {
	    $k = "'" . escape_symbol ($name);
	}
	else {
	    $k =~ s/\\/\\\\/g;
	    $k =~ s/\"/\\\"/g;
	    $k = qq("$k");
	}
	print( " $k `");
	$emacs->{'pos'} = "(gethash $k $opos)";
	print_recursive ($v);
    }
    print( ")");
}

sub GLOB::epl_print_as_lisp {
    my ($name) = get_globref_name ($_[0]);
    if (defined ($name)) {
	print( escape_symbol( $name));
	# The object is a reference but not a container.  Don't invoke
	# the circularity-tracking apparatus just because a symbol
	# appears twice in output.
	delete ($emacs->{'seen'}->{ &get_ref_id });
    }
    else {
	# XXX how here?
	warn ("got here") if $debug_stderr;
	&print_opaque;
    }
}

sub Emacs::Lisp::Variable::epl_print_as_lisp {
    print( ",");
    GLOB::epl_print_as_lisp (${$_[0]});
}

sub Emacs::Stream::epl_print_as_lisp {
    GLOB::epl_print_as_lisp (${$_[0]});
}

# Return the package and short name of the glob referred to.
sub get_globref_name {
    my $gr = shift;
    return (undef) unless UNIVERSAL::isa ($gr, 'GLOB');
    my $name = substr (*$gr, 1);  # stringify and skip "*"
    $name =~ s/^(::|main)+//;
    return ($name);
}

sub escape_symbol {
    my $name = shift;
    $name =~ tr/-_/_-/;
    $name =~ s/([^a-zA-Z0-9\-+=*\/_~!\@\$%^&:\<\>{}])/\\$1/g;
    return ($name =~ m/^-?\d+$/ ? "\\$name" : $name);
}

# CODE refs are wrapped like opaque objects but enclosed in a lambda
# expression to make them valid Lisp functions.
sub CODE::epl_print_as_lisp {
    print( ",(epl-cb-coderef ", &cb_ref_to_handle, ")");
}

sub Emacs::Lisp::Cons::epl_print_as_lisp {
    my ($value) = @_;
    my ($opos);

    $opos = $emacs->{'pos'};
    local ($emacs->{'pos'});
    print( "(");
    $emacs->{'pos'} = "(car $opos)";
    print_recursive ($$value [0]);
    print( " . ");
    $emacs->{'pos'} = "(cdr $opos)";
    print_recursive ($$value [1]);
    print( ")");
}

sub Emacs::Lisp::Cons::new {
    my ($class, %args) = @_;
    my $cons = bless ([ delete (@args{'car', 'cdr'}) ], $class);
    if (%args) {
	croak ("Emacs::Lisp::Cons::new: invalid named argument(s): "
	       . join (' ', keys (%args)));
    }
    return ($cons);
}

sub Emacs::Lisp::Cons::car { return ($_[0]->[0]); }
sub Emacs::Lisp::Cons::cdr { return ($_[0]->[1]); }
sub Emacs::Lisp::Cons::setcar { return ($_[0]->[0] = $_[1]); }
sub Emacs::Lisp::Cons::setcdr { return ($_[0]->[1] = $_[1]); }

{
    package Emacs::Lisp::Exception;

    sub new {
	my ($class, %args) = @_;
	my $self = [ delete (@args {'string', 'object'}) ];
	if (%args) {
	    croak ("Emacs::Lisp::Exception::new: invalid named argument(s): "
		   . join (' ', keys (%args)));
	}
	if (! defined ($$self [0])) {
	    $$self [0] = 'Lisp error';
	}
	return (bless ($self, $class));
    }

    sub get_object {
	return ($_[0]->[1]);
    }

    sub to_string {
	return ($_[0]->[0]);
    }
    use overload '""' => \&to_string;

    # XXX Why don't eq et al. use ""?
    sub my_cmp {
	my ($left, $right, $swapped) = @_;
	return ($swapped ? "$right" cmp "$left" : "$left" cmp "$right");
    }
    use overload 'cmp' => \&my_cmp;

    sub print_as_lisp {
	print_recursive ($_[0]->get_object);
    }
}

sub Emacs::Lisp::Opaque::epl_print_as_lisp {
    print_opaque (${$_[0]});
}

sub print_opaque {
    delete ($emacs->{'seen'}->{ &get_ref_id });
    print( ",(epl-cb-handle-to-perl ", &cb_ref_to_handle, ")");
}

# This function exists so that circular data structures can be converted.
sub fixup {
    my ($this, $that) = @_;  # this points to that.

    if ($this =~ m/^\(car (.*)\)$/s) {
	return ("(setcar $1 $that)");
    }
    if ($this =~ m/^\(cdr (.*)\)$/s) {
	return ("(setcdr $1 $that)");
    }
    if ($this =~ m/^\(aref (.*) (\d+)\)$/s) {
	return ("(aset $1 $2 $that)");
    }
    if ($this =~ m/^\(nth (\d+) (.*)\)$/) {
	return ("(setcar (nthcdr $1 $2) $that)");
    }
    if ($this =~ m/^\(gethash ("(?:\\\\|\\"|[^\\"])*\"|'(?:\\.|.)*?) (.*)\)$/)
    {
	return ("(puthash $1 $that $2)");
    }
    if ($this =~ m/^\(perl-ref (.*)\)$/) {
	return ("(perl-ref-set $1 $that)");
    }
    die ($this);
}

# Perl data referenced in Lisp.

# Subs whose names begin in "cb_" may be called by evalled messages.
# They assume that $emacs is valid.

sub cb_ref_to_handle {
    my ($id, $handle);

    $id = get_ref_id ($_[0]);
    $handle = $emacs->{'ref_id_to_handle'}->{$id};
    if (! defined ($handle)) {
	$handle = $emacs->{'next_handle'}++;
	$emacs->{'handle_to_opaque'}->{$handle} = $_[0];
	$emacs->{'ref_id_to_handle'}->{$id} = $handle;
    }
    return ($handle);
}

sub cb_handle_to_ref {
    my ($handle) = @_;
    if (exists ($emacs->{'handle_to_opaque'}->{$handle})) {
	return ($emacs->{'handle_to_opaque'}->{$handle});
    }
    else {
	die ("Stale object handle $handle\n");
    }
}

sub cb_object {
    my ($handle) = @_;
    return (bless ([ $emacs, $handle ], 'Emacs::Lisp::Object'));
}

sub cb_cons {
    my ($car, $cdr) = @_;
    return (bless [ $car, $cdr ], 'Emacs::Lisp::Cons');
}

# This is equivalent to calling &cb_unref on every handle referenced by
# $emacs except the ones given in @_.
sub cb_free_refs_except {
    my $new_h2o = {};
    my $new_i2h = {};
    while (@_) {
	my $handle = shift;
	my $obj = $emacs->{'handle_to_opaque'}->{$handle};
	$$new_h2o {$handle} = $obj;
	$$new_i2h { get_ref_id ($obj) } = $handle;
    }
    $emacs->{'handle_to_opaque'} = $new_h2o;
    $emacs->{'ref_id_to_handle'} = $new_i2h;
    return undef;  # Avoid sending a meaningless return value.
}

# Handle an UNREF type message in which Lisp promises never again to refer
# to the given handles.
sub cb_unref {
    while (@_) {
	my $handle = shift;
	my $ref = delete ($emacs->{'handle_to_opaque'}->{$handle});
	if ($ref) {
	    delete ($emacs->{'ref_id_to_handle'}->{ get_ref_id ($ref) });
	}
    }
    return undef;  # Avoid sending a meaningless return value.
}

# These `die's are never supposed to cross user code.  They are just a
# convenient way of indicating the current message type without parsing
# the message in loop_1.

# The `SKIP', however, crosses a user frame, and user code must rethrow it
# if it catches it.

sub cb_return {
    ($$emacs {'retval'}) = @_;
    die ("RETURN\n");
}

# cb_raise is a form of RAISE.  It is called with one or two arguments.
# If the error originated in Perl, its only argument is the original $@
# value.  If the error originated in Lisp, the first argument is a string
# representation, and the second argument is an error object.
sub cb_raise {
    $$emacs {'retval'} = [ @_ ];
    die ("RAISE\n");
}

# POP message type.  Called during Lisp `throw'.
# Indicates that we can no longer return normally from the current eval.
sub cb_pop {
    die ("POP\n");
}

sub send_message {
    my ($ofh, $err);

    if ($$emacs {'exited'}) {
	croak ("Emacs has exited");
    }

    $ofh = select ($$emacs {'out'});
    my $selectsaver = bless (\$ofh, 'Emacs::EPL::selectsaver');
    sub Emacs::EPL::selectsaver::DESTROY { select ${$_[0]}; }

    local $\ = "";
    local $, = "";

    if ($debug_stderr) {
	select (STDERR);
	print( "P>>> ");
	&print_stuff;
	print( "\n");
	select ($$emacs {'out'});
    }

    &print_stuff;

    # XXX emacs -batch mode uses line buffering (GNU Emacs 21.0.x)
    if ($$emacs {'pid'}) {
	print( "\n");
    }
    else {
	# Flush the stream.
	$| = 1;
	$| = 0;
    }
}

sub send_and_receive {
    &send_message;
    return loop_1 ();
}

$Emacs::next_id = 1;

sub Emacs::new {
    my $class = shift;
    my $id = $Emacs::next_id++;
    my $self = bless ({
		       'id' => $id,
		       # Hmm.  Maybe a global handle space would be nice.
		       'handle_to_opaque' => {},
		       'next_handle' => 1,
		       'ref_id_to_handle' => {},
		       @_
		      }, $class);
    $Emacs::id_to_emacs {$id} = $self;
    return ($self);
}

# XXX We should really implement our own open2 to make sure nothing
# funny happens with %ENV or %SIG.  This would further eliminate the
# restriction on module ordering, because we would use our duped
# STDIN and STDOUT instead of the possibly tied versions.  And it
# would let us play with emacs's argv[0] by using exec PROGRAM LIST.
sub my_open2 {

    # Don't ask why, because I don't know.
    *IPC::Open3::croak = \&Carp::croak;
    sub Symbol::qualify ($;$);
    *IPC::Open3::qualify = \&Symbol::qualify;
    sub Symbol::gensym ();
    *IPC::Open3::gensym = \&Symbol::gensym;

    require IPC::Open2;

    { local $^W = 0; *my_open2 = \&IPC::Open2::open2; }
    goto &my_open2;
}

# Die unless it's okay to operate on the given Emacs.
# Return true if operating on it requires setting $emacs.
sub local_check {
    return 1 if ! defined ($emacs);
    return 0 if $emacs == $_[0];
    return 1 if ! $$emacs {'in_DESTROY'};
    # This could be avoided by rewriting this module to use lexicals.
    # It is a consequence of Perl bug 20010205.006, which causes SEGV
    # in perl -e 'sub DESTROY { local $o } local $o = bless []; print;'.
    confess ("Can't operate on one Emacs while another is under destruction");
}

# Return true if it's safe to localize $emacs.
sub local_safe {
    return ((! defined ($emacs)) || (! $$emacs {'in_DESTROY'}));
}

# Avoid using %ENV in Emacs::start(), because %ENV may be tied by then.
my $ENV_EMACS = $ENV{'EMACS'};

sub Emacs::start {
    if (! local_safe ()) {
	confess ("Can't create an Emacs while another is under destruction");
    }

    my ($class) = @_;
    local (*READ, *WRITE);
    my ($prog, $vers, $where_i_am, @minus_L, $pid);

    $prog = $Emacs::program;
    if (not (defined ($prog))) {
	$prog = $ENV_EMACS;
    }
    if (not (defined ($prog))) {
	$prog = 'emacs';
    }

    # In protocol terms, this is a START message we are sending.  We are
    # the master.

    $VERSION =~ m/(\d+)\.0*(\d+)/;
    $vers = "$1.$2";

    $where_i_am = $INC{'Emacs/EPL.pm'};
    if (defined ($where_i_am) && $where_i_am =~ s,EPL\.pm\z,,) {
	push (@minus_L, $where_i_am);
    }
    if (-d ('blib') && -d ('lisp')) {
	# For the test suite.
	push (@minus_L, 'lisp');
    }
    # XXX Avoid -L args with XEmacs, which seems to ignore -L and treat
    # the next arg as a dir to open in dired mode.
    # XXX For XEmacs, set EMACSLOADPATH.  Maybe consult EPLPATH or such too.
    if ($prog =~ m,xemacs[^/]*$,) {
	@minus_L = ();
    }

    $pid = my_open2 ("READ", "WRITE", $prog, "-batch", @Emacs::args,
		     (map { ("-L", $_) } @minus_L),
		     "-l", "epl-server", $vers);

    if (! $pid) {
	croak ("Can't run $prog: $!");
    }

    local $emacs = $class->new (
				'in' => *READ,
				'out' => *WRITE,
				'pid' => $pid,
				'depth' => 0,
				'role' => 'server',
			       );

    # Wait for the handshake message.
    eval { loop_1 (); };
    if ($@) {
	$emacs->stop;
	die $@;
    }

    return ($emacs);
}

# Shut down an Emacs instance.
sub Emacs::stop {
    # Allow package method Emacs->stop to act implicitly on
    # $Emacs::current, but do nothing if not currently active.
    if (! (ref ($_[0]))) {
	if (ref ($Emacs::current)) {
	    $Emacs::current->stop;
	}
	return;
    }

    local $emacs = $_[0] if &local_check;

    return if $$emacs {'exited'};

    # If it's not our child, it's our parent.  Stopping it would result
    # in our exit.
    if ($$emacs {'role'} eq 'client') {
	croak ("Emacs::stop: can't stop my parent process; use &kill_emacs()");
    }

    if ($$emacs {'depth'} > 0) {
	# This means keep sending POP messages until depth is 0, then
	# call this function again.  And then die, because you can no
	# longer return to the Lisp function(s) you are in.
	die (".Emacs::EPL EXIT\n");
    }

    send_message ('pop');
    $emacs->free;
}

sub Emacs::free {
    local $emacs = $_[0] if &local_check;

    delete ($Emacs::id_to_emacs {$$emacs {'id'}});
    $$emacs {'exited'} = 1;
    if (defined ($Emacs::current)
	&& $Emacs::current == $emacs)
    {
	$Emacs::current = undef;
    }

    close ($$emacs {'in'});
    close ($$emacs {'out'});
    if (defined ($$emacs {'pid'})) {
	local ($?);
	waitpid ($$emacs {'pid'}, 0);
	$$emacs {'wstat'} = $?;
    }
}

sub Emacs::DESTROY {
    return if $exiting;
    my ($emacs) = @_;
    local $$emacs {'in_DESTROY'} = 1;
    local ($@);
    eval { $emacs->stop };
    if ($@) { $emacs->free; }
}

END {
    Emacs::cleanup () if defined (&Emacs::cleanup);
    $exiting = 1;
    undef ($Emacs::current);
    for my $e (values %Emacs::id_to_emacs) {
	local ($@);
	eval { $e->stop; };
	warn $@ if $@;
    }
    if (scalar (keys %Emacs::id_to_emacs) != 0) {
	warn (scalar (keys %Emacs::id_to_emacs) . " Emacs processes"
	      ." still referenced at shutdown.\n");
    }
}

sub check_version_and_args {
    my ($desired, @bad_args) = @_;

    if (defined ($desired)) {
	local $^W = 0;
	if ($desired != $VERSION) {
	    croak ("Version mismatch: $desired versus Emacs::EPL $VERSION");
	}
    }
    if (scalar (@bad_args) != 0) {
	croak ("Unknown 'use Emacs::EPL' usage: "
	       . join (', ', map { "'$_'" } @bad_args));
    }
}

sub import {
    my ($server, $version, @bad_args);

    shift;
    while (@_) {
	my $arg = shift;
	if ($arg eq ':server') {
	    $server = 1;
	}
	elsif ($arg =~ m/^\d/) {
	    $version = $arg;
	}
	else {
	    push (@bad_args, $arg);
	}
    }
    if ($server) {

	# We've received START, so we are a slave to Emacs.
	# server_init() will send RETURN, or else it will die and we'll
	# tidy up with a RAISE.

	local ($@);
	eval {
	    check_version_and_args ($version, @bad_args);
	    server_init ();
	};
	if ($@) {
	    # Some kind of error happened.  Let `perl-interpreter-new'
	    # know so that it can clean up.
	    send_message ('raise', $@);
	    exit (1);
	}
    }
    else {
	check_version_and_args ($version, @bad_args);
    }
}

# This must happen before Emacs.pm gets its grubby paws on STDIN et al.
sub server_init {
    if (tied (*STDIN) || tied (*STDOUT)) {
	croak ("STD handles tied too early,"
	       . " import Emacs::EPL before Emacs.pm");
    }

    open (OUT, ">&=" . fileno (STDOUT))
	|| die ("Can't fdopen stdout: $!");
    open (IN, "<&=" . fileno (STDIN))
	|| die ("Can't fdopen stdin: $!");
    # Emacs commingles stderr with stdout.  Bad.
    close (STDERR);

    $emacs = Emacs->new (
			 'in' => *IN,
			 'out' => *OUT,
			 # depth really starts as 1, but we are going to
			 # send a RETURN, which normally would decrement
			 # depth to 0, but we will use send_and_receive,
			 # which increments rather than decrements depth
			 # regardless of the type of message being sent.
			 'depth' => -1,
			 'role' => 'client',
			);
    $Emacs::current = $emacs;
}

# Called by epl.el (epl-interp-new).
# Talk with Emacs via this process's standard input and output.
# Use aliases so that the Perl variables STDIN and STDOUT may be tied.
# State on entry is <1>.
sub loop {

    # The RETURN lets `perl-interpreter-new' know startup succeeded.
    # This gets us from state <1> to <2,0> in the transition table.
    # Then, we loop answering requests until we get a POP message in the
    # outermost frame, which triggers return and ends communication.

    # Guard against POP from misbehaving Perl code.  That would be like
    # saying that we created our creator.
    #   Emacs: I give thee life.
    #   Emacs: Evaluate this code.
    #   Perl: I have a message for one above you.
    #   Emacs: Impossible, fool.

    # Caveat: I very much doubt that this will work in any existing
    # version of Perl.  If it were not for this attempt at
    # ultra-correctness, this function would be reduced to
    #   { send_and_receive ('return'); return; }
    my ($first, $err);
    $first = 1;
    {
	local ($@);
	my $done = 0;
	my $catch = bless (\$done, 'Emacs::EPL::loop_catch');
    AGAIN:
	eval {
	    if ($first) {
		$first = 0;
		send_and_receive ('return');
	    }
	    else {
		# Lord, I want to die.
		send_and_receive ('raise', 'Perl tried to exit');
	    }
	};
	$err = $@;
	$done = 1;
	sub Emacs::EPL::loop_catch::DESTROY {
	    if (${$_[0]} == 0) {
		# XXX Probably dies, but it's Perl's fault!
		goto AGAIN;
	    }
	}
    }
    if ($err) {
	die ($err);
    }
    # This function gives no meaningful return value.
    return;
}

sub read_error {
    my $msg = "Read error: Emacs seems to have died";
    if ($!) {
	$msg .= " ($!)";
    }
    $emacs->free;
    if ($$emacs {'wstat'}) {
	$msg .= " (wstat $$emacs{'wstat'})";
    }
    croak ($msg);
}

# Loop answering CALL and UNREF messages.  Finish when we get a RETURN,
# RAISE, POP, or EXIT.  If we get a POP, send a RETURN and raise a SKIP.
# If we get an EXIT, raise another EXIT.  But if it's the outermost
# frame, don't raise SKIP or EXIT, instead return normally.

# This function may be reentered during the handling of any type of message.
sub loop_1 {
    local $$emacs {'depth'} = $$emacs {'depth'} + 1;
    my ($input, $output, $len, $caught, $done);

    while (1) {
	local ($$emacs {'retval'});

	$len = readline ($$emacs {'in'});
	if (! defined ($len)) {
	    read_error ();
	}

	# XXX GNU Emacs 21 prints this prompt in batch mode.
	$len =~ s/^(?:Lisp expression: )+//;
	chomp ($len);
	if ($len eq '') {
	    read_error ();
	}

	if (read ($$emacs {'in'}, $input, $len) != $len) {
	    read_error ();
	}
	if ($debug_stderr) {
	    print STDERR ("P<<< $input\n");
	}

	$done = 0;
	{
	    local ($@);
	    my $catch = bless (\$done, 'Emacs::EPL::loop_1_catch');

	    # We will reenter during this eval if it happens to be a CALL
	    # or UNREF message.
	    $output = my_eval ($input);

	    $caught = $@;
	    $done = 1;
	    sub Emacs::EPL::loop_1_catch::DESTROY {
		if (! ${$_[0]}) {

		    # Oh no!  Can't stop loop_1 from exiting, so let
		    # Emacs know we're jumping.
		    # This can happen in case of 'goto'.
		    # XXX Maybe also 'exit', 'last', etc.

		    # We're not allowed to pop the final frame when Emacs is
		    # master.  loop() must convert such things into exceptions.
		    if ($$emacs {'depth'} == 1 && ! $$emacs {'pid'}) {
			return;  # Return from the destructor.
		    }

		    # All sorts of stuff might happen during this reentry.
		    send_and_receive ('pop');
		}
	    }
	}
	if ($caught) {

	    # XXX Could avoid a lot of overloaded cmp-ing and ""-ing of
	    # Lisp errors by checking ref($caught) here first.

	    if ($caught eq "RETURN\n") {
		return ($$emacs {'retval'});
	    }
	    if ($caught eq "POP\n") {
		if ($$emacs {'depth'} == 1) {
		    # If Perl is master, Lisp is not allowed to pop frame 1.
		    # It would mean an uncaught throw in epl-server.el.
		    # Hence, Emacs is master, we are being told to exit
		    # (perl-destruct), and we do so by returning from loop().
		    last;
		}
		send_message ('return');
		die (".Emacs::EPL SKIP\n");
	    }
	    if ($caught eq ".Emacs::EPL EXIT\n") {
		if ($$emacs {'depth'} == 1) {
		    $$emacs {'depth'} = 0;
		    $emacs->stop;
		    croak ("Exited a calling Emacs");
		}
		send_and_receive ('pop');
		die (".Emacs::EPL EXIT\n");
	    }
	    if ($caught eq ".Emacs::EPL SKIP\n") {
		# What this means is:  The above my_eval issued a CALL
		# back into Lisp.  In the ensuing message loop, Lisp
		# sent a POP.  We were obliged to send a RETURN and
		# discard the frame that had issued the CALL.  We did
		# so by jumping here.  Now we're back to where we were
		# when we received the original eval request.  Maybe
		# we'll have better luck next time.
		next;
	    }
	    if ($caught eq "RAISE\n") {
		my ($string, $object) = @ { $$emacs {'retval'} };
		if (defined ($object)) {
		    die (Emacs::Lisp::Exception->new
			 ( 'string' => $string, 'object' => $object ));
		}
		# Exception of Perl type.
		die ($string);
	    }

	    # By now we know that the request we are handling is a CALL,
	    # and the exception that we caught came from (or through)
	    # user code.  Send a RAISE message.
	    if (UNIVERSAL::isa ($caught, 'Emacs::Lisp::Exception')) {
		send_message ('propagate', $caught->get_object);
	    }
	    else {
		send_message ('raise', $caught);
	    }
	}
	else {
	    # The request was a CALL or UNREF, and no exception was received.
	    send_message ('return', $output);
	}
    }
}

sub local_current {
    if (! local_safe ()) {
	confess ("Can't call Lisp while an Emacs is under destruction");
    }
    return $Emacs::current ||= Emacs->start;
}

sub Emacs::Lisp::funcall {
    local $emacs = local_current ();

    if (defined (wantarray)) {
	return (send_and_receive ('call', \@_));
    }
    send_and_receive ('call-void', \@_);
}

sub Emacs::Lisp::Object::funcall {
    local $emacs = local_current ();

    if (defined (wantarray)) {
	return (send_and_receive ('call-raw', \@_));
    }
    send_and_receive ('call-void', \@_);
}

# Promise Lisp that we will not refer to this handle any more.
# Assumptions:  This can happen only during a loop_1-inspired 'eval'
# (when Emacs is waiting for a reply) or at top level (when Emacs is
# waiting for a request).  We trust the other side not to send anything
# other than UNREF messages until send_and_receive returns.
sub Emacs::Lisp::Object::DESTROY {
    my ($e, $handle) = @ { $_[0] };

    # If Perl is exiting (perhaps due to `perl-destruct'), do nothing.
    # Rely on Emacs to remember what refs we have and to free them.
    # (It should anyway, in case of abnormal subprocess termination.)
    return if $exiting;

    # If Emacs has exited (`Emacs->stop'), do nothing.  The handle is
    # already invalid.
    return if $$emacs {'exited'} || $$emacs {'in_DESTROY'};

    local $emacs = $e if local_check ($e);
    send_and_receive ('unref', [ $handle ]);
}

sub Emacs::Lisp::Object::to_perl {
    return unless defined (wantarray);
    if (! UNIVERSAL::isa ($_[0], 'Emacs::Lisp::Object')) {
	return ($_[0]);
    }
    local $emacs = $_[0]->[0] if local_check ($_[0]->[0]);
    return send_and_receive ('call', [ \*::epl_echo_one_arg, $_[0] ]);
}

sub Emacs::Lisp::lisp {
    return unless defined (wantarray);
    return (Emacs::Lisp::Object::funcall (\*::epl_echo_one_arg, $_[0]));
}

sub check_arg_count {
    my ($fn, $got, $expected) = @_;

    return if $got == $expected;
    die (Emacs::Lisp::Exception->new
	 ('string' => "Wrong number of arguments: $fn, $got",
	  'object' => [\*::wrong_number_of_arguments, \*{"::$fn"}, $got]));
}

sub Emacs::Lisp::cons {
    check_arg_count ('cons', scalar(@_), 2);
    my ($car, $cdr) = @_;

    if (defined ($cdr)) {
	return (bless ([ $car, $cdr ], 'Emacs::Lisp::Cons'));
    }
    else {
	return ([ $car ]);
    }
}

sub Emacs::Lisp::consp {
    check_arg_count ('consp', scalar(@_), 1);
    return \*::t if ref ($_[0]) eq 'ARRAY' && scalar ($_[0]) > 0;
    return \*::t if UNIVERSAL::isa ($_[0], 'Emacs::Lisp::Cons');
    return undef;
}

sub check_cons {
    my ($obj) = @_;
    return $obj if Emacs::Lisp::consp ($obj);
    die (Emacs::Lisp::Exception->new
	 ('string' => "Wrong type argument: listp, $obj",
	  'object' => [\*::wrong_type_argument, \*::listp, $obj]));
}

sub Emacs::Lisp::car {
    check_arg_count ('car', scalar(@_), 1);
    my $cons = check_cons ($_[0]);
    return $$cons [0] if ref ($cons) eq 'ARRAY';
    return ($cons->car);
}

sub Emacs::Lisp::cdr {
    check_arg_count ('cdr', scalar(@_), 1);
    my $cons = check_cons ($_[0]);
    return [ @$cons [1..$#$cons] ] if ref ($cons) eq 'ARRAY';
    return ($cons->cdr);
}

sub Emacs::Lisp::setcar {
    check_arg_count ('setcar', scalar(@_), 2);
    my ($cons, $obj) = @_;
    check_cons ($cons);
    return $$cons [0] = $obj if (ref ($cons) eq 'ARRAY');
    return ($cons->setcar ($obj));
}

sub Emacs::Lisp::setcdr {
    check_arg_count ('setcdr', scalar(@_), 2);
    my ($cons, $obj) = @_;
    check_cons ($cons);

    if (ref ($cons) eq 'ARRAY') {
	# Calling setcdr on a Perl array?  Hmm.  You get what you deserve...
	if (defined ($obj)) {
	    splice (@$cons, 1, $#$cons, $obj);
	    bless ($cons, 'Emacs::Lisp::Cons');
	}
	else {
	    @$cons [1..$#$cons] = ();
	}
    }
    else {
	$cons->setcdr ($obj);
    }
    return ($obj);
}

sub Emacs::Lisp::car_safe {
    check_arg_count ('car_safe', scalar(@_), 1);
    my ($cons) = @_;
    return (Emacs::Lisp::consp ($cons) && Emacs::Lisp::car ($cons));
}

sub Emacs::Lisp::cdr_safe {
    check_arg_count ('cdr_safe', scalar(@_), 1);
    my ($cons) = @_;
    return (Emacs::Lisp::consp ($cons) && Emacs::Lisp::cdr ($cons));
}

1;
__END__


=head1 NAME

Emacs::EPL - Protocol implementation and data conversions for Emacs Perl

=head1 SYNOPSIS

    use Emacs::EPL ':server';
    Emacs::EPL::loop;


=head1 DESCRIPTION

This module is used internally by F<epl.el> and Emacs::Lisp.

If you use C<eval> to catch errors in Lisp functions, and C<$@>
contains a string beginning with C<'.Emacs::EPL'> (note initial dot),
be sure to C<die> with the same string before returning control to
Lisp.

=head2 Protocol State Transition Table

This stuff is mainly for the benefit of the author.

    NO.   CONSTRAINTS            INITIAL       MSG CLASS    FINAL
    ----- ---------------------- ------------- ------------ --------------
    (1)                          <0>           START        <1>
    (2)                          <1>           RAISE        <0>
    (3)                          <1>           RETURN       <2,0>
    (4)                          <2,0>         RETURN       <0>
    (5)                          <2,n>         UNREF        <3,0,n>
    (6)                          <2,n>         CALL         <2,n+1>
    (7)   n>0                    <2,n>         RETURN       <2,n-1>
    (13)                         <3,0,n>       RETURN       <2,n>
    (14)                         <3,m,n>       UNREF        <3,m+1,n>
    (15)  m>0                    <3,m,n>       RETURN       <3,m-1,n>

The I<master> is defined to be the process that sends the START
message.  The other process is the I<slave>.  It follows by induction
from the table that the master sends in states <0>, <2,n> for even n,
and <3,m,n> for odd m+n, and that the slave sends in all other states.

=head2 Message Classes

=over 4

=item START

Initiate communication, e.g. by running a subprocess or opening a
connection.  The slave, if able, sends either a handshake (RETURN) or
an exception (RAISE) in response.  If an exception is raised, no
further communication is permitted.

 frame = 1

=item CALL

Request to run code.  The calling process may be reentered by a
subsequent CALL.  Our call ends when we receive a RETURN, RAISE, or
POP in the same frame or we send a POP in a next inner frame.  If we
I<receive> a POP and subsequently use RETURN to exit this frame, the
value we return will be ignored.

 frame += 1
 Lisp: funcall
 Perl: eval

=item RETURN

Deliver the response to a CALL request (7), report successful startup
(3), or mark the end of a series of UNREF requests (13, 15).  Not
permitted in a popped frame.

The three meanings could have been given different names: ``return'',
``handshake'', and ``end_unrefs''.

 frame -= 1
 Lisp: function return
 Perl: eval return

=item RAISE

Return via exception mechanism, i.e., non-locally.  RAISE has the same
protocol semantics as RETURN, except that it is permitted in popped
frames.  It is expected that unless the user has taken specific steps
(i.e., a "try" block) in the frame being returned to, the recipient
will propagate the exception by sending another RAISE with the same or
equivalent argument.

 frame -= 1
 Lisp: signal
 Perl: die

=item POP

Either terminate communication (4), or exit the current frame (11,
12).  This also says that we will ignore the argument of a subsequent
RETURN from this frame (but will not ignore a RAISE value).

 frame -= 1
 Lisp: throw, kill-emacs
 Perl: exit, any nonlocal jump other than die

=item UNREF

Send a list of handles that they have given us and that we promise
never to use again, so that they may free up some resources.  Maybe
the resources they free will include references to our stuff, so they
may send us some UNREF requests before ending the list with a RETURN.
They must not, however, issue any other kinds of requests until
they've sent RETURN in this frame.

 frame += 1
 Lisp: perl-free-ref, whatever garbage-detection means is in effect
 Perl: DESTROY

=back

=head2 Thoughts

Mark-and-sweep garbage collection could be supported by:

    (16)                         <2,n,@s>      GC           <4,n,@s>
    (17)                         <4,n,@s>      RETURN(0)    <2,n,@s>
    (18)                         <4,n,@s>      RETURN(1)    <5,0,n,@s>
    (19)                         <5,0,n,@s>    RETURN       <3,0,n,@s>
    (20)                         <5,m,n,@s>    MARK         <5,m+1,n,@s>
    (21)  m>=1                   <5,m,n,@s>    RETURN       <5,m-1,n,@s>

Transition (17) gives the receiver a chance to refuse to support
mark-and-sweep or simply to indicate that all references are in use.
Which of these two is the case could be indicated by another return
code.

It might be useful to distinguish between recursive and nonrecursive
calls:

    (22)                         <2,n>         SIMPLE_CALL  <6,n>
    (23)                         <6,n>         RETURN       <2,n>

Further state classes could be introduced to allow UNREF, GC, RAISE,
or POP operations during nonrecursive calls.  Better yet, add some
boolean parameters to the states we've got and to CALL.

Hey, how about CALL/CC and START_THREAD.  Then of course you'd need
JOIN, YIELD, LOCK, WAIT, ... .  Pretty soon you'd have yourself an
operating system.  Yawn.

The current EPL implementation uses only transitions of types (1) to
(15).


=head1 COPYRIGHT

Copyright (C) 1998-2001 by John Tobey,
jtobey@john-edwin-tobey.org.  All rights reserved.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; see the file COPYING.  If not, write to the
  Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
  MA 02111-1307  USA


=head1 SEE ALSO

L<Emacs::Lisp>, L<Emacs>.

=cut
