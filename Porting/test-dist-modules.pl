#!perl
# this should be perl 5.8 compatible, since it will be used
# with old perls while testing dist modules on those perls
#
# see C<perldoc Porting/test-dist-modules.pl>

use strict;
use warnings;
use File::Temp "tempdir";
use ExtUtils::Manifest "maniread";
use Cwd "getcwd";
use Getopt::Long;
use Config;

my $continue;
my $separate;
my $install;
my $keep;
GetOptions("c|continue" => \$continue,
           "s|separate" => \$separate,
           "i|install"  => \$install,
           "k|keep"     => \$keep,
           "h|help"     => \&usage)
  or usage("Unknown options");

$separate
  and warn "-s / -separate is now the default\n";

$|++;

-f "Configure"
  or die "Expected to be run from a perl checkout";

my $github_ci = $ENV{'GITHUB_SHA'} ? 1 : 0;

my $manifest = maniread();
my @failures = ();

my @config;
my $install_path;
unless ($install) {
    # require EU::MM 6.31 or later
    my $install_base = tempdir( CLEANUP => 1 );
    push @config, "INSTALL_BASE=$install_base";
    $ENV{PERL5LIB} .= $Config{path_sep} if $ENV{PERL5LIB};
    $ENV{PERL5LIB} .= join $Config{path_sep},
      "$install_base/lib/perl5/$Config{archname}",
      "$install_base/lib/perl5";
}

my %dist_config = (
    # these are defined by the modules as distributed on CPAN
    # I don't know why their Makefile.PLs aren't in core
    "threads"        => [ "DEFINE=-DHAS_PPPORT_H" ],
    "threads-shared" => [ "DEFINE=-DHAS_PPPORT_H" ],
   );

my $start = getcwd()
  or die "Cannot fetch current directory: $!\n";

# get ppport.h
my $pppdir = test_dist("Devel-PPPort");

if (@failures) {
    if ($github_ci) {
        # GitHub may show STDERR before STDOUT.. despite autoflush
        # being enabled.. Make sure it detects the 'endgroup' before
        # the `die` statement.
        print STDERR "::endgroup::\n";
    }
    die "Devel-PPPort failed, aborting other tests.\n";
}

my $pppfile = "$pppdir/ppport.h";

-f $pppfile
  or die "No ppport.h found in $pppdir\n";

# Devel-PPPort is manually processed before anything else to ensure we
# have an up to date ppport.h
my @dists = @ARGV;
if (@dists) {
    for my $dist (@dists) {
        -d "dist/$dist" or die "dist/$dist not a directory\n";
    }
}
else {
    opendir my $distdir, "dist"
      or die "Cannot opendir 'dist': $!\n";
    @dists = sort { lc $a cmp lc $b } grep { /^\w/ && $_ ne "Devel-PPPort" } readdir $distdir;
    closedir $distdir;
}

# These may end up being included if their problems are resolved
{
    # https://github.com/Perl/version.pm claims CPAN is upstream
    @dists = grep { $_ ne "version" } @dists;

    # Safe is tied pretty heavily to core
    # in any case it didn't seem simple to fix
    @dists = grep { $_ ne "Safe" } @dists;
}

for my $dist (@dists) {
    test_dist($dist);
}

if (@failures) {
    if ($github_ci) {
        # GitHub may show STDERR before STDOUT.. despite autoflush
        # being enabled.. Make sure it detects the 'endgroup' before
        # the `die` statement.
        print STDERR "::endgroup::\n";
    }
    my $msg = join("\n", map { "\t'$_->[0]' failed at $_->[1]" } @failures);
    die "Following dists had failures:\n$msg\n";
}

sub test_dist {
    my ($name) = @_;

    print "::group::Testing $name\n" if $github_ci;
    print "*** Testing $name ***\n";
    my $dir = tempdir( CLEANUP => !$keep);
    print "$name testing in $dir\n" if $keep;

    run("cp", "-a", "dist/$name/.", "$dir/.")
      or die "Cannot copy dist files to working directory\n";
    chdir $dir
      or die "Cannot chdir to dist working directory '$dir': $!\n";
    if ($pppfile) {
        run("cp", $pppfile, ".")
          or die "Cannot copy $pppfile to .\n";
    }
    if ($name eq "IO" || $name eq "threads" || $name eq "threads-shared") {
        write_testpl();
    }
    if ($name eq "threads" || $name eq "threads-shared") {
        write_threads_h();
    }
    if ($name eq "threads-shared") {
        write_shared_h();
    }
    unless (-f "Makefile.PL") {
        print "  Creating Makefile.PL for $name\n";
        my $key = "ABSTRACT_FROM";
        my @parts = split /-/, $name;
        my $last = $parts[-1];
        my $module = join "::", @parts;
        my $fromname;
        for my $check ("$last.pm", join("/", "lib", @parts) . ".pm") {
            if (-f $check) {
                $fromname = $check;
                last;
            }
        }
        $fromname
          or die "Cannot find ABSTRACT_FROM for $name\n";
        my $value = $fromname;
        open my $fh, ">", "Makefile.PL"
          or die "Cannot create Makefile.PL: $!\n";
        # adapted from make_ext.pl
        printf $fh <<'EOM', $module, $fromname, $key, $value;
use strict;
use ExtUtils::MakeMaker;

# This is what the .PL extracts to. Not the ultimate file that is installed.
# (ie Win32 runs pl2bat after this)

# Doing this here avoids all sort of quoting issues that would come from
# attempting to write out perl source with literals to generate the arrays and
# hash.
my @temps = 'Makefile.PL';
foreach (glob('scripts/pod*.PL')) {
    # The various pod*.PL extractors change directory. Doing that with relative
    # paths in @INC breaks. It seems the lesser of two evils to copy (to avoid)
    # the chdir doing anything, than to attempt to convert lib paths to
    # absolute, and potentially run into problems with quoting special
    # characters in the path to our build dir (such as spaces)
    require File::Copy;

    my $temp = $_;
    $temp =~ s!scripts/!!;
    File::Copy::copy($_, $temp) or die "Can't copy $temp to $_: $!";
    push @temps, $temp;
}

my $script_ext = $^O eq 'VMS' ? '.com' : '';
my %%pod_scripts;
foreach (glob('pod*.PL')) {
    my $script = $_;
    s/.PL$/$script_ext/i;
    $pod_scripts{$script} = $_;
}
my @exe_files = values %%pod_scripts;

WriteMakefile(
    NAME          => '%s',
    VERSION_FROM  => '%s',
    %-13s => '%s',
    realclean     => { FILES => "@temps" },
    (%%pod_scripts ? (
        PL_FILES  => \%%pod_scripts,
        EXE_FILES => \@exe_files,
        clean     => { FILES => "@exe_files" },
    ) : ()),
);

EOM
        close $fh;
    }

    my $verbose = $github_ci && $ENV{'RUNNER_DEBUG'} ? 1 : 0;
    my $failed = "";
    my @my_config = @config;
    if (my $cfg = $dist_config{$name}) {
        push @my_config, @$cfg;
    }
    if (!run($^X, "Makefile.PL", @my_config)) {
        $failed = "Makefile.PL";
        die "$name: Makefile.PL failed\n" unless $continue;
    }
    elsif (!run("make", "test", "TEST_VERBOSE=$verbose")) {
        $failed = "make test";
        die "$name: make test failed\n" unless $continue;
    }
    elsif (!run("make", "install")) {
        $failed = "make install";
        die "$name: make install failed\n" unless $continue;
    }

    chdir $start
      or die "Cannot return to $start: $!\n";

    if ($github_ci) {
        print "::endgroup::\n";
    }
    if ($continue && $failed) {
        print "::error ::$name failed at $failed\n" if $github_ci;
        push @failures, [ $name, $failed ];
    }

    $dir;
}

# IO, threads and threads-shared use the blead t/test.pl when tested in core
# and bundle their own test.pl when distributed on CPAN.
# The test.pl source below is from the IO distribution but so far seems sufficient
# for threads and threads-shared.
sub write_testpl {
    require File::Copy;
    require File::Spec;
    File::Copy::copy("$start/t/test.pl", "t/test.pl")
        or die "Cannot copy source t/test.pl to dist test.pl: $!";
}

# threads and threads-shared bundle this file, which isn't needed in core
sub write_threads_h {
    _write_from_data("threads.h");
}

# threads-shared bundles this file, which isn't needed in core
sub write_shared_h {
    _write_from_data("shared.h");
}

# file data read from <DATA>
my %file_data;

sub _write_from_data {
    my ($want_name) = @_;

    unless (keys %file_data) {
        my $name;
        while (<DATA>) {
            if (/^-- (\S+) --/) {
                $name = $1;
            }
            else {
                $file_data{$name} .= $_;
            }
        }
        close DATA;
    }

    my $data = $file_data{$want_name} or die "No data found for $want_name";
    open my $fh, ">", $want_name
      or die "Cannot create $want_name: $!\n";
    print $fh $data;
    close $fh
      or die "Cannot close $want_name: $!\n";
}

sub run {
    my (@cmd) = @_;

    print "\$ @cmd\n";
    my $result = system(@cmd);
    if ($result < 0) {
        print "Failed: $!\n";
    }
    elsif ($result) {
        printf "Failed: %d (%#x)\n", $result, $?;
    }
    return $result == 0;
}

sub usage {
    print <<EOS;
Usage: $^X $0 [options] [distnames]
 -c | -continue
     Continue processing after failures
     Devel::PPPort must successfully build to continue.
 -i | -install
     Install to perl's site_perl.
 -h | -help
     Display this message.

Optional distnames should be names of the distributions under dist/ to
test.  If omitted all of the distributions under dist/ are tested.
Devel-PPPort is always tested.

Test all of the distributions, stop on the first failure:

   $^X $0

Test the various threads distributions, continue on failure:

   $^X $0 -c threads threads-shared Thread-Queue Thread-Semaphore
EOS
    exit;
}

=head2 NAME

test-dist-modules.pl - test modules in dist/ against the perl invoked with

=head1 SYNOPSIS

  # from a checked out clean perl source tree
  # test all dist/ modules, abort on first failure
  path/to/perl test-dist-modules.pl

  # test all dist/ modules, continue on failure
  path/to/perl test-dist-modules.pl -c

  # test all dist/ modules, and install into path/to/perl's site_perl
  path/to/perl test-dist-modules.pl -i

=head1 DESCRIPTION

F<Porting/test-dist-modules.pl> is used by the Github workflow to test
modules from F<dist/> against the perl it is invoked with, within a
git clone of a development perl.  This clone must be a clean clone,
ie. as with C<git clean -dxf> .

That perl should have any prerequisites needed by those modules
installed, at this point this includes sufficiently recent versions
of:

 ExtUtils::MakeMaker
 Perl::OSType
 Scalar::Util
 Socket
 version

F<test-dist-modules.pl> will always test F<Devel::PPPort> first and
then use that when testing the other modules, even if invoked with a
distribution list.

=head1 INVOKING F<test-dist-modules.pl>

By default F<test-dist-modules.pl> will test each directory in
F<dist/>, but you can test specific distributions by supplying them on
the command-line:

  path/to/perl test-dist-modules.pl threads

which will test F<Devel-PPPort> and F<threads>.

Options:

=over

=item * C<-i>

=item * C<-install>

Install the modules to the invoking perl's F<site_perl>.  This may
require privileges such as running as C<root>.

=item * C<-c>

=item * C<-continue>

Continue testing modules even if one fails.

=item * C<-s>

=item * C<-separate>

Install to a temp tree instead of to the invoking perl's F<site_perl>.
This is now the default.

=item * C<-h>

=item * C<-help>

Produce a help message.

=back

=cut

__DATA__
-- threads.h --
#ifndef _THREADS_H_
#define _THREADS_H_

/* Needed for 5.8.0 */
#ifndef CLONEf_JOIN_IN
#  define CLONEf_JOIN_IN        8
#endif
#ifndef SAVEBOOL
#  define SAVEBOOL(a)
#endif

/* Added in 5.11.x */
#ifndef G_WANT
#  define G_WANT                (128|1)
#endif

/* Added in 5.24.x */
#ifndef PERL_TSA_RELEASE
#  define PERL_TSA_RELEASE(x)
#endif
#ifndef PERL_TSA_EXCLUDES
#  define PERL_TSA_EXCLUDES(x)
#endif
#ifndef CLANG_DIAG_IGNORE
#  define CLANG_DIAG_IGNORE(x)
#endif
#ifndef CLANG_DIAG_RESTORE
#  define CLANG_DIAG_RESTORE
#endif

/* Added in 5.38 */
#ifndef PERL_SRAND_OVERRIDE_NEXT_PARENT
#  define PERL_SRAND_OVERRIDE_NEXT_PARENT()
#endif

#endif
-- shared.h --
#ifndef _SHARED_H_
#define _SHARED_H_

#include "ppport.h"

#ifndef HvNAME_get
#  define HvNAME_get(hv)        (0 + ((XPVHV*)SvANY(hv))->xhv_name)
#endif

#endif
