# -*-cperl-*-
# Test conversion of cyclic data structures.

BEGIN { $| = 1; $^W = 1 }
use Emacs::Lisp;
use Data::Dumper;
$Data::Dumper::Purity = 1;

my $lx = &Emacs::Lisp::Object::read(q(

  (let ((x [nil nil nil]))
     (aset x 0 x)
     (aset x 1 (aset x 2 (cons x x)))
     x)
))->eval;

my $x = \ [undef, undef, undef];
$$x->[0] = $x;
$$x->[1] = $$x->[2] = Emacs::Lisp::Cons->new ('car' => $x, 'cdr' => $x);
$px = lisp($x);

my $py;
$py = \$py;

my $z = [];
$z->[0] = $z;
# Here it sort of breaks down due to Lisp's lack of true references.
$z->[1] = \$z->[0];
$pz = lisp($z);

@tests =
    (
     sub { ! &eq($lx, $px); },
     sub { &vectorp($lx); },
     sub { &vectorp($px); },
     sub { $lx->vectorp->to_perl; },
     sub { $px->vectorp->to_perl; },
     sub { ! $lx->consp->to_perl; },
     sub { ! $px->consp->to_perl; },
     sub { $lx->aref(0)->eq($lx)->to_perl; },
     sub { $px->aref(0)->eq($px)->to_perl; },
     sub { ! $lx->aref(1)->eq($lx)->to_perl; },
     sub { ! $px->aref(1)->eq($px)->to_perl; },
     sub { $lx->aref(1)->eq($lx->aref(2))->to_perl; },
     sub { $px->aref(1)->eq($px->aref(2))->to_perl; },
     sub { $lx->aref(1)->consp->to_perl; },
     sub { $px->aref(1)->consp->to_perl; },
     sub { $lx->aref(2)->car->eq($lx)->to_perl; },
     sub { $px->aref(2)->car->eq($px)->to_perl; },
     sub { ! $lx->perl_ref_p->to_perl; },
     sub { ! $px->perl_ref_p->to_perl; },
     sub { Dumper($lx->to_perl) eq Dumper($x); },
     sub { &perl_ref_p($py); },
     sub { &perl_ref_p(&perl_ref($py)); },
     sub { lisp($py)->perl_ref_p->to_perl; },
     sub { lisp($py)->perl_ref->perl_ref_p->to_perl; },
     sub { $pz->consp->to_perl; },
     sub { $pz->car->cdr->car->perl_ref->eq($pz); },
    );

print "1..".@tests."\n";
$test_number = 1;
for my $test (@tests) {
  print (&$test() ? "ok $test_number\n" : "not ok $test_number\n");
  $test_number ++;
}
