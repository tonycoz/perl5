HTMLROOT = /	# Change this to fix cross-references in HTML
POD2HTML_ARGS = --htmlroot=$(HTMLROOT) --podroot=.. --podpath=pod:lib:ext:vms
POD2HTML = ../ext/Pod-Html/pod2html
POD2MAN = ../cpan/podlators/pod2man
POD2TEXT = ../cpan/podlators/pod2text
PODCHECKER = ../cpan/Pod-Parser/podchecker

all: html

PERL = ..\miniperl.exe
REALPERL = ..\perl.exe

ICWD = -I..\dist\Cwd

POD = perl.pod	\
	perl5004delta.pod	\
	perl5005delta.pod	\
	perl5100delta.pod	\
	perl5101delta.pod	\
	perl5120delta.pod	\
	perl5121delta.pod	\
	perl5122delta.pod	\
	perl5123delta.pod	\
	perl5124delta.pod	\
	perl5125delta.pod	\
	perl5140delta.pod	\
	perl5141delta.pod	\
	perl5142delta.pod	\
	perl5143delta.pod	\
	perl5144delta.pod	\
	perl5160delta.pod	\
	perl5161delta.pod	\
	perl5162delta.pod	\
	perl5163delta.pod	\
	perl5180delta.pod	\
	perl5181delta.pod	\
	perl5182delta.pod	\
	perl5184delta.pod	\
	perl5200delta.pod	\
	perl5201delta.pod	\
	perl5202delta.pod	\
	perl5203delta.pod	\
	perl5220delta.pod	\
	perl5221delta.pod	\
	perl5222delta.pod	\
	perl5223delta.pod	\
	perl5224delta.pod	\
	perl5240delta.pod	\
	perl5241delta.pod	\
	perl5242delta.pod	\
	perl5243delta.pod	\
	perl5244delta.pod	\
	perl5260delta.pod	\
	perl5261delta.pod	\
	perl5262delta.pod	\
	perl5263delta.pod	\
	perl5280delta.pod	\
	perl5281delta.pod	\
	perl5282delta.pod	\
	perl5283delta.pod	\
	perl5300delta.pod	\
	perl5301delta.pod	\
	perl5302delta.pod	\
	perl5303delta.pod	\
	perl5320delta.pod	\
	perl5321delta.pod	\
	perl5340delta.pod	\
	perl5350delta.pod	\
	perl5351delta.pod	\
	perl561delta.pod	\
	perl56delta.pod	\
	perl581delta.pod	\
	perl582delta.pod	\
	perl583delta.pod	\
	perl584delta.pod	\
	perl585delta.pod	\
	perl586delta.pod	\
	perl587delta.pod	\
	perl588delta.pod	\
	perl589delta.pod	\
	perl58delta.pod	\
	perlapi.pod	\
	perlapio.pod	\
	perlartistic.pod	\
	perlbook.pod	\
	perlboot.pod	\
	perlbot.pod	\
	perlcall.pod	\
	perlcheat.pod	\
	perlclib.pod	\
	perlcommunity.pod	\
	perldata.pod	\
	perldbmfilter.pod	\
	perldebguts.pod	\
	perldebtut.pod	\
	perldebug.pod	\
	perldelta.pod	\
	perldeprecation.pod	\
	perldiag.pod	\
	perldocstyle.pod	\
	perldsc.pod	\
	perldtrace.pod	\
	perlebcdic.pod	\
	perlembed.pod	\
	perlexperiment.pod	\
	perlfilter.pod	\
	perlfork.pod	\
	perlform.pod	\
	perlfunc.pod	\
	perlgit.pod	\
	perlgov.pod	\
	perlgpl.pod	\
	perlguts.pod	\
	perlhack.pod	\
	perlhacktips.pod	\
	perlhacktut.pod	\
	perlhist.pod	\
	perlintern.pod	\
	perlinterp.pod	\
	perlintro.pod	\
	perliol.pod	\
	perlipc.pod	\
	perllexwarn.pod	\
	perllocale.pod	\
	perllol.pod	\
	perlmod.pod	\
	perlmodinstall.pod	\
	perlmodlib.pod	\
	perlmodstyle.pod	\
	perlmroapi.pod	\
	perlnewmod.pod	\
	perlnumber.pod	\
	perlobj.pod	\
	perlootut.pod	\
	perlop.pod	\
	perlopentut.pod	\
	perlpacktut.pod	\
	perlperf.pod	\
	perlpod.pod	\
	perlpodspec.pod	\
	perlpodstyle.pod	\
	perlpolicy.pod	\
	perlport.pod	\
	perlpragma.pod	\
	perlre.pod	\
	perlreapi.pod	\
	perlrebackslash.pod	\
	perlrecharclass.pod	\
	perlref.pod	\
	perlreftut.pod	\
	perlreguts.pod	\
	perlrepository.pod	\
	perlrequick.pod	\
	perlreref.pod	\
	perlretut.pod	\
	perlrun.pod	\
	perlsec.pod	\
	perlsecpolicy.pod	\
	perlsource.pod	\
	perlstyle.pod	\
	perlsub.pod	\
	perlsyn.pod	\
	perlthrtut.pod	\
	perltie.pod	\
	perltoc.pod	\
	perltodo.pod	\
	perltooc.pod	\
	perltoot.pod	\
	perltrap.pod	\
	perlunicode.pod	\
	perlunicook.pod	\
	perlunifaq.pod	\
	perluniintro.pod	\
	perluniprops.pod	\
	perlunitut.pod	\
	perlutil.pod	\
	perlvar.pod	\
	perlvms.pod

MAN = perl.man	\
	perl5004delta.man	\
	perl5005delta.man	\
	perl5100delta.man	\
	perl5101delta.man	\
	perl5120delta.man	\
	perl5121delta.man	\
	perl5122delta.man	\
	perl5123delta.man	\
	perl5124delta.man	\
	perl5125delta.man	\
	perl5140delta.man	\
	perl5141delta.man	\
	perl5142delta.man	\
	perl5143delta.man	\
	perl5144delta.man	\
	perl5160delta.man	\
	perl5161delta.man	\
	perl5162delta.man	\
	perl5163delta.man	\
	perl5180delta.man	\
	perl5181delta.man	\
	perl5182delta.man	\
	perl5184delta.man	\
	perl5200delta.man	\
	perl5201delta.man	\
	perl5202delta.man	\
	perl5203delta.man	\
	perl5220delta.man	\
	perl5221delta.man	\
	perl5222delta.man	\
	perl5223delta.man	\
	perl5224delta.man	\
	perl5240delta.man	\
	perl5241delta.man	\
	perl5242delta.man	\
	perl5243delta.man	\
	perl5244delta.man	\
	perl5260delta.man	\
	perl5261delta.man	\
	perl5262delta.man	\
	perl5263delta.man	\
	perl5280delta.man	\
	perl5281delta.man	\
	perl5282delta.man	\
	perl5283delta.man	\
	perl5300delta.man	\
	perl5301delta.man	\
	perl5302delta.man	\
	perl5303delta.man	\
	perl5320delta.man	\
	perl5321delta.man	\
	perl5340delta.man	\
	perl5350delta.man	\
	perl5351delta.man	\
	perl561delta.man	\
	perl56delta.man	\
	perl581delta.man	\
	perl582delta.man	\
	perl583delta.man	\
	perl584delta.man	\
	perl585delta.man	\
	perl586delta.man	\
	perl587delta.man	\
	perl588delta.man	\
	perl589delta.man	\
	perl58delta.man	\
	perlapi.man	\
	perlapio.man	\
	perlartistic.man	\
	perlbook.man	\
	perlboot.man	\
	perlbot.man	\
	perlcall.man	\
	perlcheat.man	\
	perlclib.man	\
	perlcommunity.man	\
	perldata.man	\
	perldbmfilter.man	\
	perldebguts.man	\
	perldebtut.man	\
	perldebug.man	\
	perldelta.man	\
	perldeprecation.man	\
	perldiag.man	\
	perldocstyle.man	\
	perldsc.man	\
	perldtrace.man	\
	perlebcdic.man	\
	perlembed.man	\
	perlexperiment.man	\
	perlfilter.man	\
	perlfork.man	\
	perlform.man	\
	perlfunc.man	\
	perlgit.man	\
	perlgov.man	\
	perlgpl.man	\
	perlguts.man	\
	perlhack.man	\
	perlhacktips.man	\
	perlhacktut.man	\
	perlhist.man	\
	perlintern.man	\
	perlinterp.man	\
	perlintro.man	\
	perliol.man	\
	perlipc.man	\
	perllexwarn.man	\
	perllocale.man	\
	perllol.man	\
	perlmod.man	\
	perlmodinstall.man	\
	perlmodlib.man	\
	perlmodstyle.man	\
	perlmroapi.man	\
	perlnewmod.man	\
	perlnumber.man	\
	perlobj.man	\
	perlootut.man	\
	perlop.man	\
	perlopentut.man	\
	perlpacktut.man	\
	perlperf.man	\
	perlpod.man	\
	perlpodspec.man	\
	perlpodstyle.man	\
	perlpolicy.man	\
	perlport.man	\
	perlpragma.man	\
	perlre.man	\
	perlreapi.man	\
	perlrebackslash.man	\
	perlrecharclass.man	\
	perlref.man	\
	perlreftut.man	\
	perlreguts.man	\
	perlrepository.man	\
	perlrequick.man	\
	perlreref.man	\
	perlretut.man	\
	perlrun.man	\
	perlsec.man	\
	perlsecpolicy.man	\
	perlsource.man	\
	perlstyle.man	\
	perlsub.man	\
	perlsyn.man	\
	perlthrtut.man	\
	perltie.man	\
	perltoc.man	\
	perltodo.man	\
	perltooc.man	\
	perltoot.man	\
	perltrap.man	\
	perlunicode.man	\
	perlunicook.man	\
	perlunifaq.man	\
	perluniintro.man	\
	perluniprops.man	\
	perlunitut.man	\
	perlutil.man	\
	perlvar.man	\
	perlvms.man

HTML = perl.html	\
	perl5004delta.html	\
	perl5005delta.html	\
	perl5100delta.html	\
	perl5101delta.html	\
	perl5120delta.html	\
	perl5121delta.html	\
	perl5122delta.html	\
	perl5123delta.html	\
	perl5124delta.html	\
	perl5125delta.html	\
	perl5140delta.html	\
	perl5141delta.html	\
	perl5142delta.html	\
	perl5143delta.html	\
	perl5144delta.html	\
	perl5160delta.html	\
	perl5161delta.html	\
	perl5162delta.html	\
	perl5163delta.html	\
	perl5180delta.html	\
	perl5181delta.html	\
	perl5182delta.html	\
	perl5184delta.html	\
	perl5200delta.html	\
	perl5201delta.html	\
	perl5202delta.html	\
	perl5203delta.html	\
	perl5220delta.html	\
	perl5221delta.html	\
	perl5222delta.html	\
	perl5223delta.html	\
	perl5224delta.html	\
	perl5240delta.html	\
	perl5241delta.html	\
	perl5242delta.html	\
	perl5243delta.html	\
	perl5244delta.html	\
	perl5260delta.html	\
	perl5261delta.html	\
	perl5262delta.html	\
	perl5263delta.html	\
	perl5280delta.html	\
	perl5281delta.html	\
	perl5282delta.html	\
	perl5283delta.html	\
	perl5300delta.html	\
	perl5301delta.html	\
	perl5302delta.html	\
	perl5303delta.html	\
	perl5320delta.html	\
	perl5321delta.html	\
	perl5340delta.html	\
	perl5350delta.html	\
	perl5351delta.html	\
	perl561delta.html	\
	perl56delta.html	\
	perl581delta.html	\
	perl582delta.html	\
	perl583delta.html	\
	perl584delta.html	\
	perl585delta.html	\
	perl586delta.html	\
	perl587delta.html	\
	perl588delta.html	\
	perl589delta.html	\
	perl58delta.html	\
	perlapi.html	\
	perlapio.html	\
	perlartistic.html	\
	perlbook.html	\
	perlboot.html	\
	perlbot.html	\
	perlcall.html	\
	perlcheat.html	\
	perlclib.html	\
	perlcommunity.html	\
	perldata.html	\
	perldbmfilter.html	\
	perldebguts.html	\
	perldebtut.html	\
	perldebug.html	\
	perldelta.html	\
	perldeprecation.html	\
	perldiag.html	\
	perldocstyle.html	\
	perldsc.html	\
	perldtrace.html	\
	perlebcdic.html	\
	perlembed.html	\
	perlexperiment.html	\
	perlfilter.html	\
	perlfork.html	\
	perlform.html	\
	perlfunc.html	\
	perlgit.html	\
	perlgov.html	\
	perlgpl.html	\
	perlguts.html	\
	perlhack.html	\
	perlhacktips.html	\
	perlhacktut.html	\
	perlhist.html	\
	perlintern.html	\
	perlinterp.html	\
	perlintro.html	\
	perliol.html	\
	perlipc.html	\
	perllexwarn.html	\
	perllocale.html	\
	perllol.html	\
	perlmod.html	\
	perlmodinstall.html	\
	perlmodlib.html	\
	perlmodstyle.html	\
	perlmroapi.html	\
	perlnewmod.html	\
	perlnumber.html	\
	perlobj.html	\
	perlootut.html	\
	perlop.html	\
	perlopentut.html	\
	perlpacktut.html	\
	perlperf.html	\
	perlpod.html	\
	perlpodspec.html	\
	perlpodstyle.html	\
	perlpolicy.html	\
	perlport.html	\
	perlpragma.html	\
	perlre.html	\
	perlreapi.html	\
	perlrebackslash.html	\
	perlrecharclass.html	\
	perlref.html	\
	perlreftut.html	\
	perlreguts.html	\
	perlrepository.html	\
	perlrequick.html	\
	perlreref.html	\
	perlretut.html	\
	perlrun.html	\
	perlsec.html	\
	perlsecpolicy.html	\
	perlsource.html	\
	perlstyle.html	\
	perlsub.html	\
	perlsyn.html	\
	perlthrtut.html	\
	perltie.html	\
	perltodo.html	\
	perltooc.html	\
	perltoot.html	\
	perltrap.html	\
	perlunicode.html	\
	perlunicook.html	\
	perlunifaq.html	\
	perluniintro.html	\
	perluniprops.html	\
	perlunitut.html	\
	perlutil.html	\
	perlvar.html	\
	perlvms.html
# not perltoc.html

TEX = perl.tex	\
	perl5004delta.tex	\
	perl5005delta.tex	\
	perl5100delta.tex	\
	perl5101delta.tex	\
	perl5120delta.tex	\
	perl5121delta.tex	\
	perl5122delta.tex	\
	perl5123delta.tex	\
	perl5124delta.tex	\
	perl5125delta.tex	\
	perl5140delta.tex	\
	perl5141delta.tex	\
	perl5142delta.tex	\
	perl5143delta.tex	\
	perl5144delta.tex	\
	perl5160delta.tex	\
	perl5161delta.tex	\
	perl5162delta.tex	\
	perl5163delta.tex	\
	perl5180delta.tex	\
	perl5181delta.tex	\
	perl5182delta.tex	\
	perl5184delta.tex	\
	perl5200delta.tex	\
	perl5201delta.tex	\
	perl5202delta.tex	\
	perl5203delta.tex	\
	perl5220delta.tex	\
	perl5221delta.tex	\
	perl5222delta.tex	\
	perl5223delta.tex	\
	perl5224delta.tex	\
	perl5240delta.tex	\
	perl5241delta.tex	\
	perl5242delta.tex	\
	perl5243delta.tex	\
	perl5244delta.tex	\
	perl5260delta.tex	\
	perl5261delta.tex	\
	perl5262delta.tex	\
	perl5263delta.tex	\
	perl5280delta.tex	\
	perl5281delta.tex	\
	perl5282delta.tex	\
	perl5283delta.tex	\
	perl5300delta.tex	\
	perl5301delta.tex	\
	perl5302delta.tex	\
	perl5303delta.tex	\
	perl5320delta.tex	\
	perl5321delta.tex	\
	perl5340delta.tex	\
	perl5350delta.tex	\
	perl5351delta.tex	\
	perl561delta.tex	\
	perl56delta.tex	\
	perl581delta.tex	\
	perl582delta.tex	\
	perl583delta.tex	\
	perl584delta.tex	\
	perl585delta.tex	\
	perl586delta.tex	\
	perl587delta.tex	\
	perl588delta.tex	\
	perl589delta.tex	\
	perl58delta.tex	\
	perlapi.tex	\
	perlapio.tex	\
	perlartistic.tex	\
	perlbook.tex	\
	perlboot.tex	\
	perlbot.tex	\
	perlcall.tex	\
	perlcheat.tex	\
	perlclib.tex	\
	perlcommunity.tex	\
	perldata.tex	\
	perldbmfilter.tex	\
	perldebguts.tex	\
	perldebtut.tex	\
	perldebug.tex	\
	perldelta.tex	\
	perldeprecation.tex	\
	perldiag.tex	\
	perldocstyle.tex	\
	perldsc.tex	\
	perldtrace.tex	\
	perlebcdic.tex	\
	perlembed.tex	\
	perlexperiment.tex	\
	perlfilter.tex	\
	perlfork.tex	\
	perlform.tex	\
	perlfunc.tex	\
	perlgit.tex	\
	perlgov.tex	\
	perlgpl.tex	\
	perlguts.tex	\
	perlhack.tex	\
	perlhacktips.tex	\
	perlhacktut.tex	\
	perlhist.tex	\
	perlintern.tex	\
	perlinterp.tex	\
	perlintro.tex	\
	perliol.tex	\
	perlipc.tex	\
	perllexwarn.tex	\
	perllocale.tex	\
	perllol.tex	\
	perlmod.tex	\
	perlmodinstall.tex	\
	perlmodlib.tex	\
	perlmodstyle.tex	\
	perlmroapi.tex	\
	perlnewmod.tex	\
	perlnumber.tex	\
	perlobj.tex	\
	perlootut.tex	\
	perlop.tex	\
	perlopentut.tex	\
	perlpacktut.tex	\
	perlperf.tex	\
	perlpod.tex	\
	perlpodspec.tex	\
	perlpodstyle.tex	\
	perlpolicy.tex	\
	perlport.tex	\
	perlpragma.tex	\
	perlre.tex	\
	perlreapi.tex	\
	perlrebackslash.tex	\
	perlrecharclass.tex	\
	perlref.tex	\
	perlreftut.tex	\
	perlreguts.tex	\
	perlrepository.tex	\
	perlrequick.tex	\
	perlreref.tex	\
	perlretut.tex	\
	perlrun.tex	\
	perlsec.tex	\
	perlsecpolicy.tex	\
	perlsource.tex	\
	perlstyle.tex	\
	perlsub.tex	\
	perlsyn.tex	\
	perlthrtut.tex	\
	perltie.tex	\
	perltoc.tex	\
	perltodo.tex	\
	perltooc.tex	\
	perltoot.tex	\
	perltrap.tex	\
	perlunicode.tex	\
	perlunicook.tex	\
	perlunifaq.tex	\
	perluniintro.tex	\
	perluniprops.tex	\
	perlunitut.tex	\
	perlutil.tex	\
	perlvar.tex	\
	perlvms.tex

man:	$(POD2MAN) $(MAN)

html:	$(POD2HTML) $(HTML)

toc:
	$(PERL) -I../lib buildtoc >perltoc.pod

.SUFFIXES: .pm .pod

.SUFFIXES: .man

.pm.man:
	$(PERL) -I../lib $(POD2MAN) $*.pm >$*.man

.pod.man:
	$(PERL) -I../lib $(POD2MAN) $*.pod >$*.man

.SUFFIXES: .html

.pm.html:
	$(PERL) -I../lib $(POD2HTML) $(POD2HTML_ARGS) --infile=$*.pm --outfile=$*.html

.pod.html:
	$(PERL) -I../lib $(POD2HTML) $(POD2HTML_ARGS) --infile=$*.pod --outfile=$*.html

clean:
	rm -f $(MAN)
	rm -f $(HTML)
	rm -f $(TEX)
	rm -f pod2html-*cache
	rm -f *.aux *.log *.exe

realclean:	clean

distclean:	realclean

check:	$(PODCHECKER)
	@echo "checking..."; \
	$(PERL) -I../lib $(PODCHECKER) $(POD)
