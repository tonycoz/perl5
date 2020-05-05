#!./perl

use Config;

BEGIN {
    if($ENV{PERL_CORE}) {
        if ($Config{'extensions'} !~ /\bIO\b/) {
	    print "1..0 # Skip: IO extension not built\n";
	    exit 0;
        }
    }
}

use Test::More tests => 7;
use IO::File;
use IO::Seekable;

$x = new_tmpfile IO::File;
ok($x, "new_tmpfile");
print $x "ok 2\n";
$x->seek(0,SEEK_SET);
my $line = <$x>;
is($line, "ok 2\n", "check we can write to the tempfile");

$x->seek(0,SEEK_SET);
print $x "not ok 3\n";
$p = $x->getpos;
print $x "ok 3\n";
$x->flush;
$x->setpos($p);
$line = <$x>;
is($line, "ok 3\n", "test getpos/setpos");

$! = 0;
$x->setpos(undef);
ok($!, "setpos(undef) makes errno non-zero");

SKIP:
{ # [perl #64772] IO::Handle->sync fails on an O_RDONLY descriptor
    $Config{d_fsync}
       or skip "No fsync", 1;
    $^O =~ /^(?:aix|irix)$/
      and skip "fsync() documented to fail on non-writable handles on $^O", 1;
    $^O eq 'cygwin'
      and skip "fsync() on cygwin uses FlushFileBuffers which requires a writable handle", 1;
    $^O eq 'VMS'
      and skip "fsync() not allowed on a read-only handle on $^O", 1;
    open my $fh, "<", "t/io_xs.t"
       or skip "Cannot open t/io_xs.t read-only: $!", 1;
    ok($fh->sync, "sync to a read only handle")
	or diag "sync(): ", $!;
}

SKIP:
{
    # gh #17455
    # the IO::Handle::blocking() implementation on Win32 was always using
    # a fd of -1.
    # A typical caller would be creating the socket using IO::Socket
    # which blesses the result into that class and that provides its own
    # blocking implementation and doesn't have this problem.
    #
    # The issue here was Win32 specific, but nothing else appears to test
    # this implementation.
    use Socket;
    ok(socket(my $sock, PF_INET, SOCK_STREAM, getprotobyname('tcp')),
       "create a socket to make non-blocking")
       or skip "couldn't create the socket: $!", 1;
    ok(defined IO::Handle::blocking($sock, 0),
       "successfully make socket non-blocking");
}
