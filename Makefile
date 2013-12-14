#-----------------------------------------------------------------------------
# PXSL Makefile
#
# CVS: $Id: Makefile,v 1.11 2005/01/27 14:43:52 thor Exp $
#
# Copyright (C) 2003-07 Tom Moertel <tom@moertel.com>
# Licensed under the terms of the GNU General Public License.
# See the LICENSE file in the project distribution for details.
#-----------------------------------------------------------------------------

PROJECT          = pxsl
VERSION     	 = 1.0.2

TARGETS     	 = dist/build/pxslcc/pxslcc

DOCDIR      	 = docs
TESTDIR          = tests
MIRRORHOST       =

SPECFILE         = pxsl-tools.spec
DISTDIR     	 = sources/$(PROJECT)-$(VERSION)
TARBALL          = $(DISTDIR).tar.gz

BINDISTDIR       = Linux-binaries/$(PROJECT)-$(VERSION)-$(shell uname -ms | tr 'A-Z ' 'a-z-')
BINTARGETS      := README $(TARGETS) examples

HASKELL         := $(wildcard src/*.hs src/*.hs.in)

TESTS_IN        := $(wildcard $(TESTDIR)/*.xsl)
TESTS_OUT_PXSL  := $(TESTS_IN:.xsl=.pxsl)
TESTS_OUT_XSL2  := $(TESTS_IN:.xsl=.xsl2)
TESTS_OUT_DIFF  := $(TESTS_IN:.xsl=.diff)
TESTS_OUT_PXSL2 := $(TESTS_IN:.xsl=.pxsl2)

DIST_MANIFEST   := README README.html LICENSE Makefile xsl2pxsl.xsl \
                   text-to-html.pl $(HASKELL) examples TODO \
                   $(SPECFILE)

GENERATED       := $(sort $(TARGETS) $(TESTS_OUT_PXSL) $(TESTS_OUT_XSL2) \
                     $(TESTS_OUT_PXSL2) $(TESTS_OUT_DIFF))

YEAR2           := $(shell date +%y)

#-----------------------------------------------------------------------------
# top level targets: all docs dist
#-----------------------------------------------------------------------------

.PHONY : all allfast allopt allstatic dist bindist

all : $(TARGETS) README.html
allfast : ; make GHC_OPT='' all
allopt : ; make GHC_OPT='-O2' all
allstatic : ; make GHC_OPT='-O2 -optl-static' all

dist : $(DISTDIR)
bindist : $(BINDISTDIR)

$(DISTDIR) : $(DIST_MANIFEST)
	mkdir -p $@
	rm -rf $@
	rsync -avC --delete-excluded $(DIST_MANIFEST) $@/
	echo $(DIST_MANIFEST) | tr ' ' \\012 > $@/MANIFEST
	touch $@
	cd $(dir $@) && \
	    rm -f $(notdir $@.tar.gz) && \
	    tar zcf $(notdir $@.tar.gz) $(notdir $@)


$(BINDISTDIR) : $(BINTARGETS)
	@[ -d $@ ] || mkdir -p $@
	rsync -avC $(BINTARGETS) $@/
	touch $@
	rm -f $@.tar.gz && tar zcf $@.tar.gz $@

$(DOCDIR) : $(HASKELL)
	@[ -d $@ ] || mkdir -p $@
	haddock --html -o $(DOCDIR) $(HASKELL) && touch $@

.PHONY : test
test : $(TESTS_OUT_PXSL2) $(TESTS_OUT_DIFF)

.PHONY : rpms
rpms : dist
	rpmbuild -ta $(TARBALL)

.PHONY : mirror
mirror : README-online.html
	rsync -e ssh -avC README-online.html $(MIRRORHOST)/pxsl/README.html
	rsync -e ssh -avRC examples sources/*.gz Linux-binaries/*.gz \
	    $(MIRRORHOST)/pxsl/
	rsync -e ssh -avC $(HOME)/rpm/RPMS/*/pxsl*rpm $(HOME)/rpm/SRPMS/pxsl*rpm \
	    $(MIRRORHOST)/pxsl/RPMS

README-online.html : README.html
	perl -lpe'BEGIN{print "<!--"} s{(<body>)}{$$1 -->}i' $< > $@


#-----------------------------------------------------------------------------
# helpers
#-----------------------------------------------------------------------------

# run tests

.SECONDARY : $(TESTS_OUT_PXSL) $(TESTS_OUT_XSL2)
%.pxsl  : %.xsl  xsl2pxsl.xsl  ; xsltproc xsl2pxsl.xsl $< > $@
%.xsl2  : %.pxsl $(TARGETS)    ; ./pxslcc -i --xslt $< > $@
%.pxsl2 : %.xsl2 xsl2pxsl.xsl  ; xsltproc xsl2pxsl.xsl $< > $@
%.diff  : %.xsl %.xsl2         ; diff -uw $^ > $@ || :

# build rules for Haskell

dist/build/pxslcc/pxslcc : $(HASKELL) Makefile
	sed -e 's/@VERSION@/$(VERSION)/' -e 's/@YEAR@/$(YEAR2)/' src/pxslcc.hs.in > src/pxslcc.hs
	runhaskell Setup.lhs configure
	runhaskell Setup.lhs build


# make README.html from README

README.html : README text-to-html.pl
	perl text-to-html.pl $< > $@

# stamp RPM spec files

%.spec : %.spec.in Makefile
	perl -pe's/\@\@VERSION\@\@/$(VERSION)/g;' $< > $@


# maintenance rules

.PHONY : clean
clean :
	rm -f *.o *.hi src/*.o src/*.hi src/*.bak $(GENERATED)
	rm -rf $(DOCDIR)

.PHONY : cleandeps
cleandeps :
	rm -f *.d

.PHONY : squeakyclean
squeakyclean : clean cleandeps

TAGS : $(HASKELL)
	hasktags $(HASKELL)
