# Emacs::EPL - used internally by Emacs Perl.

package Emacs::EPL;

# eval free of 'strict'
sub my_eval { return (eval (shift)); }

require 5.000;  # well, it's a goal anyway.

$VERSION = '0.004';
sub version_check {
    local $^W = 0;
    if ($_[0] != $VERSION) {
	die ("Version mismatch: epl.el $_[0] vs. Emacs::EPL $VERSION");
    }
}

use strict;

use vars ('$emacs');

BEGIN {
    # Set inlinable constants based on feature tests.
    local ($@);
    if (eval { require B; } && defined (&B::SVf_IOK)) {
	B->import (qw( SVf_IOK SVf_NOK ));
	eval ('sub HAVE_B () { 1 }');
    }
    else {
	eval ('sub HAVE_B () { 0 } sub SVf_IOK; sub SVf_NOK;');
	if ($@) { eval ('sub HAVE_B { 0 }'); }
    }
    'constant'->import ('HAVE_B' => ! $@);
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
	print( "(epl-cb-$callback (let ((x `");
	local $$emacs {'pos'} = "x";
	local $$emacs {'fixup'} = '';
	local $$emacs {'seen'};
	print_recursive ($_[1]);
	print( "))$$emacs{'fixup'} x))");
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
	print( ",(epl-cb-handle-to-object $handle)");
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
	print( ",(epl-cb-ref-new `");
	local $emacs->{'pos'}
	    = "(perl-ref-value $$emacs{'pos'})";
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
	my ($pkg, $name) = get_globref_stuff ($k);
	if (defined ($pkg) && $pkg eq 'main') {
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
    my ($pkg, $name) = get_globref_stuff ($_[0]);
    if (defined ($pkg)) {
	if ($pkg eq 'main') {
	    print( escape_symbol( $name));
	}
	else {
	    $name =~ s/\\/\\\\/g;
	    $name =~ s/\"/\\\"/g;
	    print( qq((perl-globref "$pkg\::$name")));
	}
	delete ($emacs->{'seen'}->{ &get_ref_id });
    }
    else {
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
sub get_globref_stuff {
    no strict 'refs';
    my $gr = shift;
    return () unless UNIVERSAL::isa ($gr, 'GLOB');
    my $str = substr (*$gr, 1);  # stringify and skip "*"
    my ($pkg, $name) = ($str =~ m/^(.*)::(.*)$/);
    return ($pkg, $name);
}

sub escape_symbol {
    my $name = shift;
    $name =~ tr/-_/_-/;
    $name =~ s/([^a-zA-Z0-9\-+=*\/_~!\@\$%^&:\<\>{}])/\\$1/g;
    return ($name);
}

# CODE refs are wrapped like opaque objects but enclosed in a lambda
# expression to make them valid Lisp functions.
sub CODE::epl_print_as_lisp {
    print( ",(epl-cb-wrap-coderef ", &cb_ref_to_handle, ")");
}

# XXX Conses.  Should inherit from Emacs::Lisp::Object?

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

sub Emacs::Lisp::Opaque::epl_print_as_lisp {
    print_opaque (${$_[0]});
}

sub print_opaque {
    delete ($emacs->{'seen'}->{ &get_ref_id });
    print( ",(epl-cb-handle-to-perl-value ", &cb_ref_to_handle, ")");
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
    if ($this =~ m/^\(perl-ref-value (.*)\)$/) {
	return ("(perl-ref-set-value $1 $that)");
    }
    if ($this eq 'x') {
	# XXX How here?
	return ("");
    }
    die ($this);
}

# Perl data referenced in Lisp.

my ($handle_to_opaque, $next_handle, $id_to_handle);

$handle_to_opaque = {};
$next_handle = 1;
$id_to_handle = {};

# Subs whose names begin in "cb_" may be called by evalled messages.
# They assume that $emacs is valid.

sub cb_ref_to_handle {
    my ($id, $handle);

    $id = get_ref_id ($_[0]);
    $handle = $$id_to_handle {$id};
    if (! defined ($handle)) {
	$handle = $next_handle++;
	$$handle_to_opaque {$handle} = $_[0];
	$$id_to_handle {$id} = $handle;
    }
    return ($handle);
}

sub cb_handle_to_ref {
    my ($handle) = @_;
    if (exists ($$handle_to_opaque {$handle})) {
	return ($$handle_to_opaque {$handle});
    }
    else {
	die ("stale object handle $handle\n");
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

sub cb_free_refs_except {
    my $new_h2o = {};
    my $new_i2h = {};
    while (@_) {
	my $handle = shift;
	my $obj = $$handle_to_opaque {$handle};
	$$new_h2o {$handle} = $obj;
	$$new_i2h {$obj + 0} = $handle;
    }
    $handle_to_opaque = $new_h2o;
    $id_to_handle = $new_i2h;
    return undef;
}

# XXX Handle the case where more than one Emacs references the same object.
sub cb_free_refs {
    while (@_) {
	my $handle = shift;
	my $ref = delete ($$handle_to_opaque {$handle});
	if ($ref) {
	    delete ($$id_to_handle { get_ref_id ($ref) });
	}
    }
    return undef;
}

sub cb_return {
    ($$emacs {'retval'}) = @_;
    die ("EPL return\n");
}

sub cb_die {
    my $msg = shift;
    $$emacs {'err'} = shift;
    die ("Lisp error: $msg");
}

sub cb_propagate {
    my ($err) = @_;
    die ($err);
}

sub cb_throw {
    die ("EPL throw\n");
}

sub cb_exit {
    $$emacs {'exiting'} = 1;
    exit;
}

sub send_message {
    my ($ofh, $err);

    $ofh = select ($$emacs {'out'});
    {
	local ($@);
	eval {
	    local $\ = "";
	    local $, = "";
	    &print_stuff;
	    if ($$emacs {'pid'}) {
		print( "\n");
	    }
	    else {
		# Flush the stream.
		$| = 1;
		$| = 0;
	    }
	};
	$err = $@;
    }
    select ($ofh);
    die ($err) if $err;
}

$Emacs::next_id = 1;

sub Emacs::new {
    my ($class, %args) = @_;
    $args {'id'} = $Emacs::next_id++;
    return ($Emacs::id_to_emacs {$args {'id'}} = bless (\%args, $class));
}

# Avoid using %ENV in Emacs::start(), because %ENV may be tied by then.
my $ENV_EMACS = $ENV{'EMACS'};

sub Emacs::start {
    my ($class) = @_;
    local (*READ, *WRITE);
    my ($prog, $pid);

    # Don't ask why, because I don't know.
    *IPC::Open3::croak = \&Carp::croak;
    sub Symbol::qualify ($;$);
    *IPC::Open3::qualify = \&Symbol::qualify;
    sub Symbol::gensym ();
    *IPC::Open3::gensym = \&Symbol::gensym;

    require IPC::Open2;

    $prog = $Emacs::program;
    if (not (defined ($prog))) {
	$prog = $ENV_EMACS;
    }
    if (not (defined ($prog))) {
	$prog = 'emacs';
    }

    # XXX We should really implement our own open2 to make sure nothing
    # funny happens with %ENV or %SIG.  This would further eliminate the
    # restriction on module ordering, because we would use our duped
    # STDIN and STDOUT instead of the possibly tied versions.

    $pid = IPC::Open2::open2 ("READ", "WRITE", $prog, "-batch", @Emacs::args,
			      # XXX Build a -L path from $Config{...}.
			      # For XEmacs, set EMACSLOADPATH.
			      "-L", "lisp",  # XXX for the testsuite.
			      "-l", "epl-server") || die $!;
    return Emacs->new (
		       'in' => *READ,
		       'out' => *WRITE,
		       'frame' => 'top',
		       'pid' => $pid,
		      );
}

sub Emacs::stop {
    my ($e) = @_;

    if (! (ref ($e))) {
	$e = $Emacs::current || return;
    }
    if ($$e {'pid'}) {
	local $emacs = $e;
	send_message ('exit');
	waitpid ($$e {'pid'}, 0);
	delete ($$e {'pid'});
    }
    delete ($Emacs::id_to_emacs {$$e {'id'}});
}

sub Emacs::DESTROY {
    my ($e) = @_;
    local ($@);
    eval { $e->stop; };
    eval { delete ($Emacs::id_to_emacs {$$e {'id'}}); };
}

END {
    Emacs::cleanup () if defined (&Emacs::cleanup);
    @Emacs::id_to_emacs { keys (%Emacs::id_to_emacs) } = ();
    $Emacs::current = undef;
    if (scalar (keys %Emacs::id_to_emacs) != 0) {
	warn (scalar (keys %Emacs::id_to_emacs) . " Emacs processes"
	      ." still referenced at shutdown.\n");
    }
}

#$SIG{'__DIE__'} = sub { warn (@_); die (@_); };

sub import {
    my ($server);

    shift;
    while (@_) {
	my $arg = shift;
	if ($arg eq ':server') {
	    $server = 1;
	}
	elsif ($arg =~ m/^\d/) {
	    version_check ($arg);
	}
	else {
	    require Carp;
	    Carp::croak ("Unknown 'use Emacs::EPL' arg: $arg");
	}
    }
    if ($server) {
	local ($@);
	eval { server_init (); };
	if ($@) {
	    # Some kind of error happened.  Let `perl-interpreter-new' know
	    # so that it can clean up.
	    send_message ('return', $@);
	    exit (1);
	}
    }
}

# This must happen before Emacs.pm gets its grubby paws on STDIN et al.
sub server_init {
    open (OUT, ">&=" . fileno (STDOUT))
	|| die ("Can't fdopen stdout: $!");
    open (IN, "<&=" . fileno (STDIN))
	|| die ("Can't fdopen stdin: $!");
    # Emacs commingles stderr with stdout.  Bad.
    close (STDERR);

    $emacs = Emacs->new (
			 'in' => *IN,
			 'out' => *OUT,
			 'frame' => 'lisp',
			);
    $Emacs::current = $emacs;

    # Let `perl-interpreter-new' know startup succeeded.
    send_message ('return', undef);
}

# Called by epl.el (perl-interpreter-new).
# Talk with Emacs via this process's standard input and output.
# Use aliases so that the Perl variables STDIN and STDOUT may be tied.
sub loop {
    my $in = $$emacs {'in'};

    while (1) {
	my ($input, $output, $len, $caught, $ofh);

	local ($$emacs {'retval'});
	local ($$emacs {'err'});

	$len = readline ($in);
	$len =~ s/^(?:Lisp expression: )+//;
	chomp ($len);
	# XXX handle errors more robustly.
	read ($in, $input, $len) == $len || die;
	{
	    local ($@);
	    # XXX trap 'goto', 'last', 'next', 'return', 'redo', 'exit'?
	    $output = my_eval ($input);
	    $caught = $@;
	}
	if ($caught) {
	    if ($caught eq "EPL skip\n") {
		next;
	    }
	    if ($caught eq "EPL throw\n") {
		die ("EPL skip\n");
	    }
	    if ($$emacs {'frame'} eq 'perl') {
		if ($caught eq "EPL return\n") {
		    return ($$emacs {'retval'});
		}
		die ($caught);
	    }
	    if (defined ($$emacs {'err'})) {
		send_message ('propagate', $$emacs {'err'});
	    }
	    else {
		send_message ('error', $caught);
	    }
	    next;
	}
	send_message ('return', $output);
    }
}

sub Emacs::Lisp::funcall {
    local $emacs = $Emacs::current ||= Emacs->start;
    local $$emacs {'frame'} = 'perl';
    # XXX Check wantarray to avoid extra refs/conversions in scalar/void.
    send_message ('funcall', \@_);
    return loop ();
}

sub Emacs::Lisp::Object::funcall {
    #my ($e);
    # Use the first argument's emacs if it has one, e.g. in $x->car.
    # Of course, this means &cons($x,$y) uses $x's emacs even if $y's
    # is $Emacs::current.  Hmmm.
    #if (defined ($_[1]) && UNIVERSAL::isa ($_[1], 'Emacs::Lisp::Object')) {
	#$e = $_[1]->[0];
    #}
    #else {
	#$e = $Emacs::current ||= Emacs->start;
    #}
    #local $emacs = $e;
    local $emacs = $Emacs::current ||= Emacs->start;
    local $$emacs {'frame'} = 'perl';
    # XXX Check wantarray to avoid extra refs/conversions in scalar/void.
    send_message ('funcall-raw', \@_);
    return loop ();
}

sub Emacs::Lisp::Object::DESTROY {
    my ($e, $handle) = @ { $_[0] };
    return if $$e {'exiting'};
    local ($@);
    local $emacs = $e;
    eval {  # Ignore errors.
	send_message ('unref-objects', $handle);
    };
}

sub Emacs::Lisp::Object::to_perl {
    if (! UNIVERSAL::isa ($_[0], 'Emacs::Lisp::Object')) {
	Carp::croak ("Argument to to_perl is not an Emacs Lisp object");
    }
    local $emacs = $_[0]->[0];
    local $$emacs {'frame'} = 'perl';
    send_message ('unwrap', $_[0]);
    return loop ();
}

sub Emacs::Lisp::lisp {
    $Emacs::current ||= Emacs->start;
    local $emacs = $Emacs::current;
    local $$emacs {'frame'} = 'perl';
    send_message ('wrap', $_[0]);
    return loop ();
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
