#!/usr/bin/perl
# Habit . . .
#
# Extract info from Config.VMS, and add extra data here, to generate Config.sh
# Edit the static information after __END__ to reflect your site and options
# that went into your perl binary.  In addition, values which change from run
# to run may be supplied on the command line as key=val pairs.
#
# Rev. 23-Apr-1996  Charles Bailey  bailey@genetics.upenn.edu
#

#==== Locations of installed Perl components
$prefix='perl_root';
$builddir="$prefix:[000000]";
$installbin="$prefix:[000000]";
$installman1dir="$prefix:[man.man1]";
$installman3dir="$prefix:[man.man3]";
$installprivlib="$prefix:[lib]";

unshift(@INC,'lib');  # In case someone didn't define Perl_Root
                      # before the build

if ($ARGV[0] eq '-f') {
  open(ARGS,$ARGV[1]) or die "Can't read data from $ARGV[1]: $!\n";
  @ARGV = ();
  while (<ARGS>) {
    push(@ARGV,split(/\|/,$_));
  }
  close ARGS;
}

if (-f "config.vms") { $infile = "config.vms"; $outdir = "[-]"; }
elsif (-f "[.vms]config.vms") { $infile = "[.vms]config.vms"; $outdir = "[]"; }
elsif (-f "config.h") { $infile = "config.h"; $outdir = "[]";}

if ($infile) { print "Generating Config.sh from $infile . . .\n"; }
else { die <<EndOfGasp;
Can't find config.vms or config.h to read!
	Please run this script from the perl source directory or
	the VMS subdirectory in the distribution.
EndOfGasp
}
$outdir = '';
open(IN,"$infile") || die "Can't open $infile: $!\n";
open(OUT,">${outdir}Config.sh") || die "Can't open ${outdir}Config.sh: $!\n";

$time = localtime;
$cf_by = (getpwuid($<))[0];
$archsufx = `Write Sys\$Output F\$GetSyi("HW_MODEL")` > 1024 ? 'AXP' : 'VAX';
($vers = $]) =~ tr/./_/;
$installarchlib = VMS::Filespec::vmspath($installprivlib);
$installarchlib =~ s#\]#.VMS_$archsufx.$vers\]#;
($osvers = `Write Sys\$Output F\$GetSyi("VERSION")`) =~ s/^V?(\S+)\s*\n?$/$1/;

print OUT <<EndOfIntro;
# This file generated by GenConfig.pl on a VMS system.
# Input obtained from:
#     $infile
#     $0
# Time: $time

package='perl5'
CONFIG='true'
cf_time='$time'
cf_by='$cf_by'
ccdlflags=''
cccdlflags=''
mab=''
libpth='/sys\$share /sys\$library'
ld='Link'
lddlflags='/Share'
ranlib=''
ar=''
eunicefix=':'
hint='none'
hintfile=''
intsize='4'
alignbytes='8'
shrplib='define'
usemymalloc='n'
usevfork='true'
useposix='false'
spitshell='write sys\$output '
dlsrc='dl_vms.c'
binexp='$installbin'
man1ext='rno'
man3ext='rno'
arch='VMS_$archsufx'
archname='VMS_$archsufx'
osvers='$osvers'
prefix='$prefix'
builddir='$builddir'
installbin='$installbin'
installman1dir='$installman1dir'
installman3dir='$installman3dir'
installprivlib='$installprivlib'
installarchlib='$installarchlib'
EndOfIntro

foreach (@ARGV) {
  ($key,$val) = split('=',$_,2);
  if ($key eq 'cc') {  # Figure out which C compiler we're using
    my($cc,$ccflags) = split('/',$val,2);
    my($d_attr);
    $ccflags = "/$ccflags";
    if ($ccflags =~s!/DECC!!ig) { 
      $cc .= '/DECC';
      $cctype = 'decc';
      $d_attr = 'undef';
    }
    elsif ($ccflags =~s!/VAXC!!ig) {
      $cc .= '/VAXC';
      $cctype = 'vaxc';
      $d_attr = 'undef';
    }
    elsif (`$val/NoObject/NoList _nla0:/Version` =~ /GNU C version (\S+)/) {
      $cctype = 'gcc';
      $d_attr = 'define';
      print OUT "gccversion='$1'\n";
    }
    elsif ($archsufx eq 'VAX' &&
           `$val/NoObject/NoList /prefix=all _nla0:` =~ /IVQUAL/) {
      $cctype = 'vaxc';
      $d_attr = 'undef';
    }
    else {
      $cctype = 'decc';
      $d_attr = 'undef';
    }
    print OUT "vms_cc_type='$cctype'\n";
    print OUT "d_attribut='$d_attr'\n";
    print OUT "cc='$cc'\n";
    if ( ($cctype eq 'decc' and $archsufx eq 'VAX') || $cctype eq 'gcc') {
      # gcc and DECC for VAX requires filename in /object qualifier, so we
      # have to remove it here.  Alas, this means we lose the user's
      # object file suffix if it's not .obj.
      $ccflags =~ s#/obj(?:ect)?=[^/\s]+##i;
    }
    print OUT "ccflags='$ccflags'\n";
    $dosock = ($ccflags =~ m!/DEF[^/]+VMS_DO_SOCKETS!i and
               $ccflags !~ m!/UND[^/]+VMS_DO_SOCKETS!i);
    print OUT "d_vms_do_sockets=",$dosock ? "'define'\n" : "'undef'\n";
    print OUT "d_socket=",$dosock ? "'define'\n" : "'undef'\n";
    print OUT "d_sockpair=",$dosock ? "'define'\n" : "'undef'\n";
    print OUT "d_gethent=",$dosock ? "'define'\n" : "'undef'\n";
    print OUT "d_select=",$dosock ? "'define'\n" : "'undef'\n";
    print OUT "i_niin=",$dosock ? "'define'\n" : "'undef'\n";
    print OUT "i_neterrno=",$dosock ? "'define'\n" : "'undef'\n";
    next;
  }
  elsif ($key eq 'exe_ext') { 
    my($nodot) = $val;
    $nodot =~ s!\.!!;
    print OUT "so='$nodot'\ndlext='$nodot'\n";
  }
  elsif ($key eq 'obj_ext') { print OUT "dlobj='dl_vms$val'\n";     }
  print OUT "$key='$val'\n";
}

# Are there any other logicals which TCP/IP stacks use for the host name?
$myname = $ENV{'ARPANET_HOST_NAME'}  || $ENV{'INTERNET_HOST_NAME'} ||
          $ENV{'MULTINET_HOST_NAME'} || $ENV{'UCX$INET_HOST'}      ||
          $ENV{'TCPWARE_DOMAINNAME'} || $ENV{'NEWS_ADDRESS'};
if (!$myname) {
  ($myname) = `hostname` =~ /^(\S+)/;
  if ($myname =~ /IVVERB/) {
    warn "Can't determine TCP/IP hostname" if $dosock;
    $myname = '';
  }
}
$myname = $ENV{'SYS$NODE'} unless $myname;
($myhostname,$mydomain) = split(/\./,$myname,2);
print OUT "myhostname='$myhostname'\n" if $myhostname;
if ($mydomain) {
  print OUT "mydomain='.$mydomain'\n";
  print OUT "perladmin='$cf_by\@$myhostname.$mydomain'\n";
  print OUT "cf_email='$cf_by\@$myhostname.$mydomain'\n";
}
else {
  print OUT "perladmin='$cf_by'\n";
  print OUT "cf_email='$cf_by'\n";
}
chomp($hwname = `Write Sys\$Output F\$GetSyi("HW_NAME")`);
$hwname = $archsufx if $hwname =~ /IVKEYW/;  # *really* old VMS version
print OUT "myuname='VMS $myname $osvers $hwname'\n";

# Before we read the C header file, find out what config.sh constants are
# equivalent to the C preprocessor macros
if (open(SH,"${outdir}config_h.SH")) {
  while (<SH>) {
    next unless m%^#(?!if).*\$%;
    s/^#//; s!(.*?)\s*/\*.*!$1!;
    my(@words) = split;
    $words[1] =~ s/\(.*//;  # Clip off args from macro
    # Did we use a shell variable for the preprocessor directive?
    if ($words[0] =~ m!^\$(\w+)!) { $pp_vars{$words[1]} = $1; }
    if (@words > 2) {  # We may also have a shell var in the value
      shift @words;              #  Discard preprocessor directive
      my($token) = shift @words; #  and keep constant name
      my($word);
      foreach $word (@words) {
        next unless $word =~ m!\$(\w+)!;
        $val_vars{$token} = $1;
        last;
      }
    }
  }
  close SH;
}
else { warn "Couldn't read ${outdir}config_h.SH: $!\n"; }
$pp_vars{UNLINK_ALL_VERSIONS} = 'd_unlink_all_versions';  # VMS_specific

# OK, now read the C header file, and retcon statements into config.sh
while (<IN>) {  # roll through the comment header in Config.VMS
  last if /config-start/;
}

while (<IN>) {
  chop;
  while (/\\\s*$/) {  # pick up contination lines
    my $line = $_;
    $line =~ s/\\\s*$//;
    $_ = <IN>;
    s/^\s*//;
    $_ = $line . $_;
  }              
  next unless my ($blocked,$un,$token,$val) =
                 m%^(\/\*)?\s*\#\s*(un)?def\w*\s+([A-Za-z0-9]\w+)\S*\s*(.*)%;
  if (/config-skip/) {
    delete $pp_vars{$token} if exists $pp_vars{$token};
    delete $val_vars{$token} if exists $val_vars{$token};
    next;
  }
  $val =~ s!\s*/\*.*!!; # strip off trailing comment
  my($had_val); # Maybe a macro with args that we just #undefd or commented
  if (!length($val) and $val_vars{$token} and ($un || $blocked)) {
    print OUT "$val_vars{$token}=''\n";
    delete $val_vars{$token};
    $had_val = 1;
  }
  $state = ($blocked || $un) ? 'undef' : 'define';
  if ($pp_vars{$token}) {
    print OUT "$pp_vars{$token}='$state'\n";
    delete $pp_vars{$token};
  }
  elsif (not length $val and not $had_val) {
    # Wups -- should have been shell var for C preprocessor directive
    warn "Constant $token not found in config_h.SH\n";
    $token =~ tr/A-Z/a-z/;
    $token = "d_$token" unless $token =~ /^i_/;
    print OUT "$token='$state'\n";
  }
  next unless length $val;
  $val =~ s/^"//; $val =~ s/"$//;               # remove end quotes
  $val =~ s/","/ /g;                            # make signal list look nice
  # Library directory; convert to VMS syntax
  $val = VMS::Filespec::vmspath($val) if ($token =~ /EXP$/);
  if ($val_vars{$token}) {
    print OUT "$val_vars{$token}='$val'\n";
    if ($val_vars{$token} =~ s/exp$//) {print OUT "$val_vars{$token}='$val'\n";}
    delete $val_vars{$token};
  }
  elsif (!$pp_vars{$token}) {  # Haven't seen it previously, either
    warn "Constant $token not found in config_h.SH (val=|$val|)\n";
    $token =~ tr/A-Z/a-z/;
    print OUT "$token='$val'\n";
    if ($token =~ s/exp$//) {print OUT "$token='$val'\n";}
  }
}
close IN;
# Special case -- preprocessor manifest "VMS" is defined automatically
# on VMS systems, but is also used erroneously by the Perl build process
# as the manifest for the obsolete variable $d_eunice.
print OUT "d_eunice='undef'\n";  delete $pp_vars{VMS};

foreach (sort keys %pp_vars) {
  warn "Didn't see $_ in $infile\n";
}
foreach (sort keys %val_vars) {
  warn "Didn't see $_ in $infile(val)\n";
}

if (open(OPT,"${outdir}crtl.opt")) {
  while (<OPT>) {
    next unless m#/(sha|lib)#i;
    chomp;
    if (/crtl/i || /gcclib/i) { push(@crtls,$_); }
    else                      { push(@libs,$_);  }
  }
  close OPT;
  print OUT "libs='",join(' ',@libs),"'\n";
  push(@crtls,'(DECCRTL)') if $cctype eq 'decc';
  print OUT "libc='",join(' ',@crtls),"'\n";
}
else { warn "Can't read ${outdir}crtl.opt - skipping 'libs' & 'libc'"; }

if (open(PL,"${outdir}patchlevel.h")) {
  while (<PL>) {
    if    (/^#define PATCHLEVEL\s+(\S+)/) { print OUT "PATCHLEVEL='$1'\n"; }
    elsif (/^#define SUBVERSION\s+(\S+)/) { print OUT "SUBVERSION='$1'\n"; }
  }
  close PL;
}
else { warn "Can't read ${outdir}patchlevel.h - skipping 'PATCHLEVEL'"; }

# simple pager support for perldoc                                             
if    (`most` =~ /IVVERB/) {
  $pager = 'more';
  if (`more nl:` =~ /IVVERB/) { $pager = 'type/page'; }
}
else { $pager = 'most'; }
print OUT "pager='$pager'\n";

close OUT;
