package Emacs::EPL;

# eval free of 'strict'
sub my_eval { return (eval (shift)); }

require 5.000;  # well, it's a goal anyway.

use strict;

BEGIN {
    local ($@);
    eval { require B; };
    if ($@) {
	eval ('sub HAVE_B () { 0 }');
	if ($@) { eval ('sub HAVE_B { 0 }'); }
    }
    else {
	eval ('sub HAVE_B () { 1 }');
    }
    constant->import ('HAVE_B' => ! $@);
    eval { require overload; };
    if ($@) { eval ('sub overload::StrVal { return "$_[0]"; }'); }
}

BEGIN {
    if (HAVE_B()) {
	B->import (qw( SVf_IOK SVf_NOK ));
    }
}

# Tell whether a scalar is "really" an int, float, or string.
# XXX Ints that are too big for Lisp should be treated as floats.
sub guess_lisp_type {
    if (HAVE_B()) {
	my $fl = B::svref_2object (\$_[0]) ->FLAGS;
	if (($fl & SVf_IOK) != 0) {
	    return ('integer');
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
	    return ('integer');
	}
	elsif ($_[0] =~ m/^-?\d+(?:\.\d+)(?:e-?\d+)$/) {
	    return ('float');
	}
	else {
	    return ('string');
	}
    }
}

# print_stuff (TAG, VALUE)
sub print_stuff {
    if (tied ($_[1]) || ref ($_[1])) {
	my (%state);

	print( "(epl-cb-$_[0] (let ((x `");
	$state {'pos'} = "x";
	$state {'fixup'} = '';
	print_recursive ($_[1], \%state);
	print( "))$state{'fixup'} x))");
    }
    else {
	# Optimize obviously non-circular cases.  (for visual aesthetics)
	print( "(epl-cb-$_[0] ");
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

# print_recursive(VALUE, STATE)
sub print_recursive {
    # Avoid unnecessary FETCH if tied.
    # Avoid unnecessary string copy.
    my $state = $_[1];
    my ($ref);

    if (tied ($_[0])) {
	if (tied ($_[0]) ->can ("PRINT_AS_LISP")) {
	    tied ($_[0]) ->PRINT_AS_LISP ($_[1]);
	    return;
	}
	my $value = $_[0];
	$ref = \$value;
    }
    else {
	$ref = \$_[0];
    }
    if (ref ($$ref)) {
	my ($value, $state) = @_;
	my ($id, $pos);

	$id = get_ref_id ($$ref);
	$pos = $state->{'seen'}->{$id};
	if (defined ($pos)) {
	    $state->{'fixup'} .= fixup ($state->{'pos'}, $pos);
	    print( "nil");
	}
	else {
	    $state->{'seen'}->{$id} = $state->{'pos'};
	    # This is like C<$$ref->epl_print_as_lisp($state)>
	    # but accepts unblessed $$ref.
	    & { ref ($$ref) ->can ('epl_print_as_lisp') } ($$ref, $state);
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
	    if ($$ref =~ m/[\\\"]/) {
		# About to modify, so copy if we have not done so yet.
		# XXX Convert everything to syswrite() to avoid copy?
		if (! tied ($_[0])) {
		    my $value = $$ref;
		    $ref = \$value;
		}
		$$ref =~ s/([\\\"])/\\$1/g;
	    }
	    print( '"', $$ref, '"');
	}
    }
    else {
	print( "nil");
    }
}

sub UNIVERSAL::epl_print_as_lisp {
    my ($value, $state) = @_;
    my ($package, $realtype, $meth);

    ($package, $realtype) = get_ref_types ($value);
    $package = $realtype if ! defined ($package);
    $meth = $package->can ('epl_print_as_lisp');

    if ($meth == \&UNIVERSAL::epl_print_as_lisp) {
	&print_opaque;
    }
    else {
	$package =~ s/\\/\\\\/g;
	$package =~ s/\"/\\\"/g;
	print( "(perl-blessed \"$package\" . ");
	local $state->{'pos'} = "(cdr (cdr $$state{'pos'}))";
	&$meth;
	print( ")");
    }
}

sub Emacs::Lisp::Object::epl_print_as_lisp {
    print( ",(epl-cb-handle-to-object ", ${$_[0]}, ")");
}

sub SCALAR::epl_print_as_lisp {
    my ($value, $state) = @_;

    print( ",(epl-cb-ref-new `");
    local $state->{'pos'} = "(perl-ref-value $$state{'pos'})";
    print_recursive ($$value, $state);
    print( ")");
}

sub REF::epl_print_as_lisp {
    my ($value, $state) = @_;
    my ($class);

    $value = $$value;
    $class = ref ($value);
    if ($class eq 'ARRAY') {
	# ref-to-ref-to-array is a Lisp vector.
	my $opos = $state->{'pos'};
	local ($state->{'pos'});
	print( "[");
	for (my $i = 0; $i <= $#$value; $i++) {
	    $state->{'pos'} = "(aref $opos $i)";
	    print( " ") if $i > 0;
	    print_recursive ($$value [$i], $state);
	}
	print( "]");
    }
    else {
	&SCALAR::epl_print_as_lisp;
    }
}

sub ARRAY::epl_print_as_lisp {
    my ($value, $state) = @_;
    my ($opos);

    $opos = $state->{'pos'};
    local ($state->{'pos'});
    print( "(");
    for (my $i = 0; $i <= $#$value; $i++) {
	$state->{'pos'} = "(nth $i $opos)";
	print( " ") if $i > 0;
	print_recursive ($$value [$i], $state);
    }
    print( ")");
}

sub HASH::epl_print_as_lisp {
    my ($value, $state) = @_;
    my ($opos);

    $opos = $state->{'pos'};
    local ($state->{'pos'});
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
	$state->{'pos'} = "(gethash $k $opos)";
	print_recursive ($v, $state);
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
    }
    else {
	&print_opaque;
    }
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
    $name =~ s/([^a-zA-Z0-9\-+=*\/_~!@\$%^&:<>{}])/\\$1/g;
    return ($name);
}

# CODE refs are wrapped like opaque objects but enclosed in a lambda
# expression to make them valid Lisp functions.
sub CODE::epl_print_as_lisp {
    print( ",(epl-cb-wrap-coderef ", &ref_to_handle, ")");
}

# XXX Conses.  Should inherit from Emacs::Lisp::Object?

sub Emacs::Lisp::Cons::epl_print_as_lisp {
    my ($value, $state) = @_;
    my ($opos);

    $opos = $state->{'pos'};
    local ($state->{'pos'});
    print( "(");
    $state->{'pos'} = "(car $opos)";
    print_recursive ($$value [0], $state);
    print( " . ");
    $state->{'pos'} = "(cdr $opos)";
    print_recursive ($$value [1], $state);
    print( ")");
}

sub Emacs::Lisp::Opaque::epl_print_as_lisp {
    print_opaque (${$_[0]}, $_[1]);
}

sub print_opaque {
    print( ",(epl-cb-handle-to-perl-value ", &ref_to_handle, ")");
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
    die;
}

# Perl data referenced in Lisp.

my ($handle_to_opaque, $next_handle, $id_to_handle);
$handle_to_opaque = {};
$next_handle = 1;
$id_to_handle = {};

sub ref_to_handle {
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

# Subs whose names begin in "cb_" may be called by evalled messages.

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
    return (bless (\$handle, 'Emacs::Lisp::Object'));
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

sub send_message {
    my ($ofh);

    $ofh = select (STDOUT);
    &print_stuff;
    # Flush the stream.
    $| = 1;
    $| = 0;
    select ($ofh);
}

# Called by perl-core.el (make-perl-interpreter).
sub loop {
    open (STDERR, ">" . (@_ ? $_[0] : "/dev/null"));
    loop_1 (0);
}

sub loop_1 {
    my ($from_perl) = @_;

    while (1) {
	my ($input, $output, $len, $err, $ofh, $returning);

	$len = <STDIN>;
	chomp ($len);
	# XXX handle errors more robustly.
	read (STDIN, $input, $len) == $len || die;
	$returning = $input =~ s/^return //;
	{
	    local ($@);
	    # XXX trap 'goto', 'last', 'next', 'return'?
	    $output = my_eval ($input);
	    $err = $@;
	}
	if ($from_perl) {
	    die ($err) if $err;
	    return ($output) if $returning;
	}
	if ($err) {
	    send_message ('error', $err);
	}
	else {
	    send_message ('return', $output);
	}
    }
}

sub Emacs::Lisp::funcall {
    send_message ('funcall', \@_);
    return loop_1 (1);
}

sub Emacs::Lisp::Object::funcall {
    send_message ('funcall-raw', \@_);
    return loop_1 (1);
}

sub Emacs::Lisp::Object::DESTROY {
    local ($@);
    eval {  # Ignore errors.
	send_message ('unref-object', ${$_[0]});
    };
}

sub Emacs::Lisp::Object::to_perl {
    send_message ('unwrap', $_[0]);
    return loop_1 (1);
}

sub Emacs::Lisp::lisp {
    send_message ('wrap', $_[0]);
    return loop_1 (1);
}

1;
__END__


=head1 NAME

Emacs::EPL - Protocol implementation and data conversions for EPL

=head1 SYNOPSIS

    use Emacs::EPL;
    Emacs::EPL::loop;


=head1 DESCRIPTION

This module is used internally by F<epl.el>.


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
