# This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
# starting from 18 December 2012 and WeiDU 231.06.

# Note added due to LGPL terms.
#
# This file was edited by Valerio Bigiani, AKA The Bigg, starting from
# 6 November 2005. All changes for this file are listed in
# diffs/Makefile.diff file, as the output of a diff -Bw -c -N command.
#
# It was originally taken from Westley Weimer's WeiDU 185.

############################################################################
# Makefile for WeiDU
############################################################################



############################################################################
# If you're an old user and try to compile WeiDU yourself, you need to edit
# the file 'Configuration' rather than this one.
############################################################################

include Configuration

# Just a target to be used by default
.PHONY: weidu doc all
all : weidu
# "make weinstall" if you want weinstall

####
#### OCAML stuff
####
# Debugging. Set ECHO= to debug this Makefile
# ECHO := @

RELEASE    := 1
NATIVECAML := 1
# UNSAFE     := 1

# Put here all the byproducts of make
OBJDIR      := obj/$(ARCHOS)
DEPENDDIR   := obj/.depend

include Depends

CAMLFLAGS      += -I zlib -I xdiff

   # Now the rule to make WeiDU

PROJECT_EXECUTABLE = $(OBJDIR)/weidu$(EXE)
PROJECT_MODULES    = $(WEIDU_MODULES)
ifdef HAVE_MINGW
PROJECT_CMODULES   = reg
PROJECT_CLIBS      = -ladvapi32
endif
#PROJECT_CMODULES   += eff_strings
PROJECT_CMODULES   += zlib adler32 inflate uncompr inftrees zutil inffast $(GLOB) xdiff
PROJECT_CMODULES   += xemit xpatchi xutils xdiffi xprepare $(ARCH_C_FILES)
PROJECT_CMODULES   += crc32 compress deflate trees

PROJECT_OCAML_LIBS = unix str #OCaml changed libstr into libcamlstr and "you are not supposed to link with -lstr"
PROJECT_LIBS       = unix camlstr

PROJECT_RESOURCES = weidu_resources

.PHONY: weidu
weidu:  $(PROJECT_EXECUTABLE)
$(PROJECT_EXECUTABLE) : $(PROJECT_MODULES:%=$(OBJDIR)/%.$(CMO)) \
                        $(PROJECT_CMODULES:%=$(OBJDIR)/%.$(OBJEXT))
	@$(NARRATIVE) Linking $(COMPILETOWHAT) $@
	i686-w64-mingw32-windres.exe -i windows_resources/weidu_resources.rc -o $(OBJDIR)/weidu_resources.o

	$(CAMLLINK) -o $@ \
                    $(PROJECT_OCAML_LIBS:%=%.$(CMXA)) \
                    $(PROJECT_LIBS:%=-cclib -l%) \
                    $(PROJECT_CLIBS:%=-cclib %) \
					$(PROJECT_RESOURCES:%=$(OBJDIR)/%.$(OBJEXT)) \
                    $^
	cp $(PROJECT_EXECUTABLE) .

# rule for tolower

PROJECT3_EXECUTABLE = $(OBJDIR)/tolower$(EXE)
PROJECT3_MODULES    = batList batteriesInit tolower
PROJECT3_CMODULES   =
PROJECT3_OCAML_LIBS = unix str
PROJECT3_LIBS       = unix camlstr
.PHONY: tolower
tolower: $(PROJECT3_EXECUTABLE)
$(PROJECT3_EXECUTABLE) : $(PROJECT3_MODULES:%=$(OBJDIR)/%.$(CMO)) \
                        $(PROJECT3_CMODULES:%=$(OBJDIR)/%.$(OBJEXT))
	@$(NARRATIVE) Linking $(COMPILETOWHAT) $@
	$(CAMLLINK) -o $@ \
                    $(PROJECT3_OCAML_LIBS:%=%.$(CMXA)) \
                    $(PROJECT3_LIBS:%=-cclib -l%) \
                    $(PROJECT3_CLIBS:%=-cclib %) \
                    $^
	cp $(PROJECT3_EXECUTABLE) .

# rule for WeInstall

PROJECT4_EXECUTABLE = $(OBJDIR)/weinstall$(EXE)
PROJECT4_MODULES    = batList batteriesInit case_ins weinstall
PROJECT4_CMODULES   =
PROJECT4_OCAML_LIBS = unix str
PROJECT4_LIBS       = unix camlstr
.PHONY: weinstall
weinstall: $(PROJECT4_EXECUTABLE)
$(PROJECT4_EXECUTABLE) : $(PROJECT4_MODULES:%=$(OBJDIR)/%.$(CMO)) \
                        $(PROJECT4_CMODULES:%=$(OBJDIR)/%.$(OBJEXT))
	@$(NARRATIVE) Linking $(COMPILETOWHAT) $@
	$(CAMLLINK) -o $@ \
                    $(PROJECT4_OCAML_LIBS:%=%.$(CMXA)) \
                    $(PROJECT4_LIBS:%=-cclib -l%) \
                    $(PROJECT4_CLIBS:%=-cclib %) \
                    $^
	cp $(PROJECT4_EXECUTABLE) .

###
### Cleaning
###
clean:
	rm -f $(PROJECT_EXECUTABLE) $(PROJECT_EXECUTABLE2) $(PROJECT_EXECUTABLE3) \
	$(PROJECT_EXECUTABLE4)  src/arch.ml src/arch2.ml src/case_ins.ml \
	src/*parser*.ml src/*parser*.mli src/*lexer*.ml src/*lexer*.mli src/*.cmi \
	src/tlexer.mll src/trealparserin.gr  \
	src/toldlexer.mll src/tph.ml
	find obj -exec rm {} \; || true


###
### Distro
###
VER = $(shell grep "let version" src/version.ml | cut -d \" -f 2 | sed -e's/\(...\)00/\1/g')
VERBIG = $(shell grep "let version" src/version.ml | cut -d \" -f 2)
doc: doc/base.tex
	$(MAKE) -C doc

windows_zip : weidu weinstall tolower
	test -d ../WeiDU-Windows || mkdir ../WeiDU-Windows
	mv -f weid*.exe ../WeiDU-Windows/weidu.exe || true
	mv -f wein*.exe ../WeiDU-Windows/weinstall.exe || true
	mv -f tolo*.exe ../WeiDU-Windows/tolower.exe || true
	strip ../WeiDU-Windows/weidu.exe || true
	upx --best ../WeiDU-Windows/weidu.exe || echo "No EXE Compression"
	strip ../WeiDU-Windows/weinstall.exe || true
	upx --best ../WeiDU-Windows/weinstall.exe || echo "No EXE Compression"
	strip ../WeiDU-Windows/tolower.exe || true
	upx --best ../WeiDU-Windows/tolower.exe || echo "No EXE Compression"
	cp README* ../WeiDU-Windows
	cp COPYING ../WeiDU-Windows
	cp -r examples ../WeiDU-Windows
	#cp windows_manifests/*.manifest ../WeiDU-Windows
	(cd .. ; zip -9r WeiDU-Windows-$(VER).zip WeiDU-Windows)
src_zip : clean
	(cd .. ; zip -9r WeiDU-Src-$(VER).zip weidu/* -x weidu/*.exe -x weidu/*.dll -x */.DS_Store; )
linux_zip : weidu weinstall tolower
	test -d ../WeiDU-Linux || mkdir ../WeiDU-Linux
	mv weid*$(EXE) ../WeiDU-Linux/weidu || true
	mv wein*$(EXE) ../WeiDU-Linux/weinstall || true
	mv tolower$(EXE) ../WeiDU-Linux/tolower || true
	strip ../WeiDU-Linux/weidu || true
	upx --best ../WeiDU-Linux/weidu || echo "No EXE Compression"
	strip ../WeiDU-Linux/weinstall || true
	upx --best ../WeiDU-Linux/weinstall || echo "No EXE Compression"
	strip ../WeiDU-Linux/tolower || true
	upx --best ../WeiDU-Linux/tolower || echo "No EXE Compression"
	cp README* ../WeiDU-Linux
	cp COPYING ../WeiDU-Linux
	cp -r examples ../WeiDU-Linux
	(cd .. ; zip -9r WeiDU-Linux-$(VER).zip WeiDU-Linux )
osx_zip : weidu weinstall
	test -d ../WeiDU-Mac || mkdir ../WeiDU-Mac
	mv weid*$(EXE) ../WeiDU-Mac/weidu || true
	mv wein*$(EXE) ../WeiDU-Mac/weinstall || true
	strip ../WeiDU-Mac/weidu || true
	strip ../WeiDU-Mac/weinstall || true
	upx --best ../WeiDU-Mac/weidu || echo "No EXE Compression"
	upx --best ../WeiDU-Mac/weinstall || echo "No EXE Compression"
	cp README* ../WeiDU-Mac
	cp COPYING ../WeiDU-Mac
	cp -r examples ../WeiDU-Mac
	#sed -e's/version_plist=.*/version_plist=\"${VERBIG}\"/g'  '../WeiDU-Mac/WeiDU Installer.command' > t
	#mv t ../WeiDU-Mac/WeiDU\ Installer.command
	(cd .. ; zip -9r WeiDU-Mac-$(VER).zip WeiDU-Mac -x */.DS_Store )

FORCE:
