#!./perl
#

# This is a home for regular expression tests that (a) don't fit into the
# format supported by re/regexp.t and (b) which are appropriate for running by
# 'miniperl'.  If you want to add a test that does fit that format and is not
# appropriate for miniperl, add it to re/pat.t.

# If you want to add a test that does fit the format supported by re/regexp.t,
# add it to re/re_tests, not here.

use strict;
use warnings;
no warnings 'experimental::vlb';
use 5.010;

sub run_tests;

$| = 1;


BEGIN {
    chdir 't' if -d 't';
    require Config; import Config;
    require './test.pl'; require './charset_tools.pl';
    require './loc_tools.pl';
    set_up_inc('../lib', '.', '../ext/re');
}

skip_all_without_unicode_tables();

plan tests => 153;  # Update this when adding/deleting tests.

run_tests() unless caller;

#
# Tests start here.
#
sub run_tests {
    my $sharp_s = uni_to_native("\xdf");

    {
        my $x = "abc\ndef\n";
	(my $x_pretty = $x) =~ s/\n/\\n/g;

        ok $x =~ /^abc/,  qq ["$x_pretty" =~ /^abc/];
        ok $x !~ /^def/,  qq ["$x_pretty" !~ /^def/];

        # used to be a test for $*
        ok $x =~ /^def/m, qq ["$x_pretty" =~ /^def/m];

        ok(!($x =~ /^xxx/), qq ["$x_pretty" =~ /^xxx/]);
        ok(!($x !~ /^abc/), qq ["$x_pretty" !~ /^abc/]);

         ok $x =~ /def/, qq ["$x_pretty" =~ /def/];
        ok(!($x !~ /def/), qq ["$x_pretty" !~ /def/]);

         ok $x !~ /.def/, qq ["$x_pretty" !~ /.def/];
        ok(!($x =~ /.def/), qq ["$x_pretty" =~ /.def/]);

         ok $x =~ /\ndef/, qq ["$x_pretty" =~ /\\ndef/];
        ok(!($x !~ /\ndef/), qq ["$x_pretty" !~ /\\ndef/]);
    }

    {
        $_ = '123';
        ok /^([0-9][0-9]*)/, qq [\$_ = '$_'; /^([0-9][0-9]*)/];
    }

    {
        $_ = 'aaabbbccc';
         ok /(a*b*)(c*)/ && $1 eq 'aaabbb' && $2 eq 'ccc',
                                             qq [\$_ = '$_'; /(a*b*)(c*)/];
         ok /(a+b+c+)/ && $1 eq 'aaabbbccc', qq [\$_ = '$_'; /(a+b+c+)/];
        unlike($_, qr/a+b?c+/, qq [\$_ = '$_'; /a+b?c+/]);

        $_ = 'aaabccc';
         ok /a+b?c+/, qq [\$_ = '$_'; /a+b?c+/];
         ok /a*b?c*/, qq [\$_ = '$_'; /a*b?c*/];

        $_ = 'aaaccc';
         ok /a*b?c*/, qq [\$_ = '$_'; /a*b?c*/];
        unlike($_, qr/a*b+c*/, qq [\$_ = '$_'; /a*b+c*/]);

        $_ = 'abcdef';
         ok /bcd|xyz/, qq [\$_ = '$_'; /bcd|xyz/];
         ok /xyz|bcd/, qq [\$_ = '$_'; /xyz|bcd/];
         ok m|bc/*d|,  qq [\$_ = '$_'; m|bc/*d|];
         ok /^$_$/,    qq [\$_ = '$_'; /^\$_\$/];
    }

    {
        # used to be a test for $*
        ok "ab\ncd\n" =~ /^cd/m, q ["ab\ncd\n" =~ /^cd/m];
    }

    {
        our %XXX = map {($_ => $_)} 123, 234, 345;

        our @XXX = ('ok 1','not ok 1', 'ok 2','not ok 2','not ok 3');
        while ($_ = shift(@XXX)) {
            my $e = index ($_, 'not') >= 0 ? '' : 1;
            my $r = m?(.*)?;
            is($r, $e, "?(.*)?");
            /not/ && reset;
            if (/not ok 2/) {
                if ($^O eq 'VMS') {
                    $_ = shift(@XXX);
                }
                else {
                    reset 'X';
                }
            }
        }

        SKIP: {
            if ($^O eq 'VMS') {
                skip "Reset 'X'", 1;
            }
            ok !keys %XXX, "%XXX is empty";
        }

    }

    {
        my $message = "Test empty pattern";
        my $xyz = 'xyz';
        my $cde = 'cde';

        $cde =~ /[^ab]*/;
        $xyz =~ //;
        is($&, $xyz, $message);

        my $foo = '[^ab]*';
        $cde =~ /$foo/;
        $xyz =~ //;
        is($&, $xyz, $message);

        $cde =~ /$foo/;
        my $null;
        no warnings 'uninitialized';
        $xyz =~ /$null/;
        is($&, $xyz, $message);

        $null = "";
        $xyz =~ /$null/;
        is($&, $xyz, $message);

        # each entry: regexp, match string, $&, //o match success
        my @tests =
          (
           [ "", "xy", "x", 1 ],
           [ "y", "yz", "y", !1 ],
          );
        for my $test (@tests) {
            my ($re, $str, $matched, $omatch) = @$test;
            $xyz =~ /x/o;
            ok($str =~ /$re/, "$str matches /$re/");
            is($&, $matched, "on $matched");
            $xyz =~ /x/o;
            is($str =~ /$re/o, $omatch, "$str matches /$re/o (or not)");
        }
    }

    {
        my $message = q !Check $`, $&, $'!;
        $_ = 'abcdefghi';
        /def/;        # optimized up to cmd
        is("$`:$&:$'", 'abc:def:ghi', $message);

        no warnings 'void';
        /cde/ + 0;    # optimized only to spat
        is("$`:$&:$'", 'ab:cde:fghi', $message);

        /[d][e][f]/;    # not optimized
        is("$`:$&:$'", 'abc:def:ghi', $message);
    }

    {
        $_ = 'now is the {time for all} good men to come to.';
        / \{([^}]*)}/;
        is($1, 'time for all', "Match braces");
    }

    {
        my $message = "{N,M} quantifier";
        $_ = 'xxx {3,4}  yyy   zzz';
        ok(/( {3,4})/, $message);
        is($1, '   ', $message);
        unlike($_, qr/( {4,})/, $message);
        ok(/( {2,3}.)/, $message);
        is($1, '  y', $message);
        ok(/(y{2,3}.)/, $message);
        is($1, 'yyy ', $message);
        unlike($_, qr/x {3,4}/, $message);
        unlike($_, qr/^xxx {3,4}/, $message);
    }

    {
        my $message = "Test /g";
        local $" = ":";
        $_ = "now is the time for all good men to come to.";
        my @words = /(\w+)/g;
        my $exp   = "now:is:the:time:for:all:good:men:to:come:to";

        is("@words", $exp, $message);

        @words = ();
        while (/\w+/g) {
            push (@words, $&);
        }
        is("@words", $exp, $message);

        @words = ();
        pos = 0;
        while (/to/g) {
            push(@words, $&);
        }
        is("@words", "to:to", $message);

        pos $_ = 0;
        @words = /to/g;
        is("@words", "to:to", $message);
    }

    {
        $_ = "abcdefghi";

        my $pat1 = 'def';
        my $pat2 = '^def';
        my $pat3 = '.def.';
        my $pat4 = 'abc';
        my $pat5 = '^abc';
        my $pat6 = 'abc$';
        my $pat7 = 'ghi';
        my $pat8 = '\w*ghi';
        my $pat9 = 'ghi$';

        my $t1 = my $t2 = my $t3 = my $t4 = my $t5 =
        my $t6 = my $t7 = my $t8 = my $t9 = 0;

        for my $iter (1 .. 5) {
            $t1++ if /$pat1/o;
            $t2++ if /$pat2/o;
            $t3++ if /$pat3/o;
            $t4++ if /$pat4/o;
            $t5++ if /$pat5/o;
            $t6++ if /$pat6/o;
            $t7++ if /$pat7/o;
            $t8++ if /$pat8/o;
            $t9++ if /$pat9/o;
        }
        my $x = "$t1$t2$t3$t4$t5$t6$t7$t8$t9";
        is($x, '505550555', "Test /o");
    }

    {
        my $xyz = 'xyz';
        ok "abc" =~ /^abc$|$xyz/, "| after \$";

        # perl 4.009 says "unmatched ()"
        my $message = '$ inside ()';

        my $result;
        eval '"abc" =~ /a(bc$)|$xyz/; $result = "$&:$1"';
        is($@, "", $message);
        is($result, "abc:bc", $message);
    }

    {
        my $message = "Scalar /g";
        $_ = "abcfooabcbar";

        ok( /abc/g && $` eq "", $message);
        ok( /abc/g && $` eq "abcfoo", $message);
        ok(!/abc/g, $message);

        $message = "Scalar /gi";
        pos = 0;
        ok( /ABC/gi && $` eq "", $message);
        ok( /ABC/gi && $` eq "abcfoo", $message);
        ok(!/ABC/gi, $message);

        $message = "Scalar /g";
        pos = 0;
        ok( /abc/g && $' eq "fooabcbar", $message);
        ok( /abc/g && $' eq "bar", $message);

        $_ .= '';
        my @x = /abc/g;
        is(@x, 2, "/g reset after assignment");
    }

    {
        my $message = '/g, \G and pos';
        $_ = "abdc";
        pos $_ = 2;
        /\Gc/gc;
        is(pos $_, 2, $message);
        /\Gc/g;
        is(pos $_, undef, $message);
    }

    {
        my $message = '(?{ })';
        our $out = 1;
        'abc' =~ m'a(?{ $out = 2 })b';
        is($out, 2, $message);

        $out = 1;
        'abc' =~ m'a(?{ $out = 3 })c';
        is($out, 1, $message);
    }

    {
        $_ = 'foobar1 bar2 foobar3 barfoobar5 foobar6';
        my @out = /(?<!foo)bar./g;
        is("@out", 'bar2 barf', "Negative lookbehind");
    }

    {
        my $message = "REG_INFTY tests";
        # Tests which depend on REG_INFTY

	#  Defaults assumed if this fails
	eval { require Config; };
        $::reg_infty   = $Config::Config{reg_infty} // 65535;
        $::reg_infty_m = $::reg_infty - 1;
        $::reg_infty_p = $::reg_infty + 1;
        $::reg_infty_m = $::reg_infty_m;   # Suppress warning.

        # As well as failing if the pattern matches do unexpected things, the
        # next three tests will fail if you should have picked up a lower-than-
        # default value for $reg_infty from Config.pm, but have not.

        is(eval q{('aaa' =~ /(a{1,$::reg_infty_m})/)[0]}, 'aaa', $message);
        is($@, '', $message);
        is(eval q{('a' x $::reg_infty_m) =~ /a{$::reg_infty_m}/}, 1, $message);
        is($@, '', $message);
        isnt(q{('a' x ($::reg_infty_m - 1)) !~ /a{$::reg_infty_m}/}, 1, $message);
        is($@, '', $message);

        eval "'aaa' =~ /a{1,$::reg_infty}/";
        like($@, qr/^\QQuantifier in {,} bigger than/, $message);
        eval "'aaa' =~ /a{1,$::reg_infty_p}/";
        like($@, qr/^\QQuantifier in {,} bigger than/, $message);

        # It should be 'a' x 2147483647, but that exhausts memory on
        # reasonably sized modern machines
        like('a' x $::reg_infty_p, qr/a{1,}/,
             "{1,} matches more times than REG_INFTY");
    }

    {
        # Poke a couple more parse failures
        my $context = 'x' x 256;
        eval qq("${context}y" =~ /(?<=$context)y/);
        ok $@ =~ /^\QLookbehind longer than 255 not/, "Lookbehind limit";
    }

  SKIP:
    {   # Long Monsters

        skip('limited memory', 20) if $ENV{'PERL_SKIP_BIG_MEM_TESTS'};

        for my $l (125, 140, 250, 270, 300000, 30) { # Ordered to free memory
            my $a = 'a' x $l;
	    my $message = "Long monster, length = $l";
	    like("ba$a=", qr/a$a=/, $message);
            unlike("b$a=", qr/a$a=/, $message);
            like("b$a=", qr/ba+=/, $message);

	    like("ba$a=", qr/b(?:a|b)+=/, $message);
        }
    }

  SKIP:
    {   # 20000 nodes, each taking 3 words per string, and 1 per branch

        skip('limited memory', 20) if $ENV{'PERL_SKIP_BIG_MEM_TESTS'};

        my $long_constant_len = join '|', 12120 .. 32645;
        my $long_var_len = join '|', 8120 .. 28645;
        my %ans = ( 'ax13876y25677lbc' => 1,
                    'ax13876y25677mcb' => 0, # not b.
                    'ax13876y35677nbc' => 0, # Num too big
                    'ax13876y25677y21378obc' => 1,
                    'ax13876y25677y21378zbc' => 0,    # Not followed by [k-o]
                    'ax13876y25677y21378y21378kbc' => 1,
                    'ax13876y25677y21378y21378kcb' => 0, # Not b.
                    'ax13876y25677y21378y21378y21378kbc' => 0, # 5 runs
                  );

        for (keys %ans) {
	    my $message = "20000 nodes, const-len '$_'";
            ok !($ans{$_} xor /a(?=([yx]($long_constant_len)){2,4}[k-o]).*b./o), $message;

	    $message = "20000 nodes, var-len '$_'";
            ok !($ans{$_} xor /a(?=([yx]($long_var_len)){2,4}[k-o]).*b./o,), $message;
        }
    }

    {
        my $message = "Complicated backtracking";
        $_ = " a (bla()) and x(y b((l)u((e))) and b(l(e)e)e";
        my $expect = "(bla()) ((l)u((e))) (l(e)e)";

        our $c;
        sub matchit {
          m/
             (
               \(
               (?{ $c = 1 })    # Initialize
               (?:
                 (?(?{ $c == 0 })   # PREVIOUS iteration was OK, stop the loop
                   (?!
                   )        # Fail: will unwind one iteration back
                 )
                 (?:
                   [^()]+        # Match a big chunk
                   (?=
                     [()]
                   )        # Do not try to match subchunks
                 |
                   \(
                   (?{ ++$c })
                 |
                   \)
                   (?{ --$c })
                 )
               )+        # This may not match with different subblocks
             )
             (?(?{ $c != 0 })
               (?!
               )        # Fail
             )            # Otherwise the chunk 1 may succeed with $c>0
           /xg;
        }

        my @ans = ();
        my $res;
        push @ans, $res while $res = matchit;
        is("@ans", "1 1 1", $message);

        @ans = matchit;
        is("@ans", $expect, $message);

        $message = "Recursion with (??{ })";
        our $matched;
        $matched = qr/\((?:(?>[^()]+)|(??{$matched}))*\)/;

        @ans = my @ans1 = ();
        push (@ans, $res), push (@ans1, $&) while $res = m/$matched/g;

        is("@ans", "1 1 1", $message);
        is("@ans1", $expect, $message);

        @ans = m/$matched/g;
        is("@ans", $expect, $message);

    }

    {
        ok "abc" =~ /^(??{"a"})b/, '"abc" =~ /^(??{"a"})b/';
    }

    {
        my @ans = ('a/b' =~ m%(.*/)?(.*)%);    # Stack may be bad
        is("@ans", 'a/ b', "Stack may be bad");
    }

    {
        my $message = "Eval-group not allowed at runtime";
        my $code = '{$blah = 45}';
        our $blah = 12;
        eval { /(?$code)/ };
        ok($@ && $@ =~ /not allowed at runtime/ && $blah == 12, $message);

	$blah = 12;
	my $res = eval { "xx" =~ /(?$code)/o };
	{
	    no warnings 'uninitialized';
	    chomp $@; my $message = "$message '$@', '$res', '$blah'";
	    ok($@ && $@ =~ /not allowed at runtime/ && $blah == 12, $message);
	}

        $code = '=xx';
	$blah = 12;
	$res = eval { "xx" =~ /(?$code)/o };
	{
	    no warnings 'uninitialized';
	    my $message = "$message '$@', '$res', '$blah'";
	    ok(!$@ && $res, $message);
	}

        $code = '{$blah = 45}';
        $blah = 12;
        eval "/(?$code)/";
        is($blah, 45, $message);

        $blah = 12;
        /(?{$blah = 45})/;
        is($blah, 45, $message);
    }

    {
        my $message = "Pos checks";
        my $x = 'banana';
        $x =~ /.a/g;
        is(pos $x, 2, $message);

        $x =~ /.z/gc;
        is(pos $x, 2, $message);

        sub f {
            my $p = $_[0];
            return $p;
        }

        $x =~ /.a/g;
        is(f (pos $x), 4, $message);
    }

    {
        my $message = 'Checking $^R';
        our $x = $^R = 67;
        'foot' =~ /foo(?{$x = 12; 75})[t]/;
        is($^R, 75, $message);

        $x = $^R = 67;
        'foot' =~ /foo(?{$x = 12; 75})[xy]/;
        ok($^R eq '67' && $x eq '12', $message);

        $x = $^R = 67;
        'foot' =~ /foo(?{ $^R + 12 })((?{ $x = 12; $^R + 17 })[xy])?/;
        ok($^R eq '79' && $x eq '12', $message);
    }

    {
        is(qr/\b\v$/i,    '(?^i:\b\v$)', 'qr/\b\v$/i');
        is(qr/\b\v$/s,    '(?^s:\b\v$)', 'qr/\b\v$/s');
        is(qr/\b\v$/m,    '(?^m:\b\v$)', 'qr/\b\v$/m');
        is(qr/\b\v$/x,    '(?^x:\b\v$)', 'qr/\b\v$/x');
        is(qr/\b\v$/xism, '(?^msix:\b\v$)',  'qr/\b\v$/xism');
        is(qr/\b\v$/,     '(?^:\b\v$)', 'qr/\b\v$/');
    }

    {   # Test that charset modifier work, and are interpolated
        is(qr/\b\v$/, '(?^:\b\v$)', 'Verify no locale, no unicode_strings gives default modifier');
        is(qr/(?l:\b\v$)/, '(?^:(?l:\b\v$))', 'Verify infix l modifier compiles');
        is(qr/(?u:\b\v$)/, '(?^:(?u:\b\v$))', 'Verify infix u modifier compiles');
        is(qr/(?l)\b\v$/, '(?^:(?l)\b\v$)', 'Verify (?l) compiles');
        is(qr/(?u)\b\v$/, '(?^:(?u)\b\v$)', 'Verify (?u) compiles');
    }


} # End of sub run_tests

1;

#
# ex: set ts=8 sts=4 sw=4 et:
#
