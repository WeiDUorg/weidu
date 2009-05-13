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
# "make weimorph" if you want iwg2
# "make weigui" if you want weigui
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

# First stuff that makes the executable
# to add later:
WEIDU_BASE_MODULES  := case_ins stats arch version parsing tph util modder var arch2 xor \
	eff_table key cbif biff tlk load cre \
        ids idslexer idslexer2 idsparser \
        bcs bcslexer bcsparser \
        dlg dc \
        baflexer bafparser \
        diff sav mos tp dlexer dparser \
        smutil useract lexerint parsetables arraystack objpool glr lrparse \
        automate kit tpstate tphelp trealparser tlexer toldparser toldlexer tparser parsewrappers  tppe tpuninstall tppatch tpaction tpwork
ifdef ITEMLIST
WEIDU_BASE_MODULES  += pretty itemlist
endif
WEIDU_MODULES  =  myarg $(WEIDU_BASE_MODULES) autoupdate main

MODULES        = $(WEIDU_MODULES) iwgconf iwgrule iwglexer iwgparser iwg2 myarg weinstall tolower
SOURCEDIRS     := src zlib glob xdiff elkhound
MLLS           := dlexer.mll idslexer.mll idslexer2.mll bcslexer.mll baflexer.mll iwglexer.mll tlexer.mll toldlexer.mll
MLYS           := dparser.mly idsparser.mly bcsparser.mly bafparser.mly iwgparser.mly toldparser.mly
GRS            := trealparserin.gr

$(OBJDIR)/mytop :
	ocamlmktop -o ${OBJDIR}/mytop str.cma unix.cma

src/tph.ml : src/tph/include/* src/tph/define/* src/make_tph.ml $(OBJDIR)/mytop
	./${OBJDIR}/mytop -w p src/make_tph.ml

$(OBJDIR)/trealparser.ml : $(OBJDIR)/trealparserin.ml $(OBJDIR)/mytop src/make_par.ml
	cp $(OBJDIR)/trealparserin.ml $(OBJDIR)/trealparser.ml

$(OBJDIR)/trealparser.mli : $(OBJDIR)/trealparserin.mli
	cp $(OBJDIR)/trealparserin.mli $(OBJDIR)/trealparser.mli

src/tlexer.mll : src/tlexer.in src/trealparserin.gr src/make_tll.ml $(OBJDIR)/mytop
	./${OBJDIR}/mytop -w p src/make_tll.ml

src/trealparserin.gr : src/trealparserin.in src/aliases.in src/make_gr.ml $(OBJDIR)/mytop
	./${OBJDIR}/mytop -w p src/make_gr.ml

src/toldlexer.mll : src/toldlexer.in src/trealparserin.gr src/make_old_mll.ml $(OBJDIR)/mytop
	./${OBJDIR}/mytop -w p src/make_old_mll.ml

src/arch.ml : src/$(ARCH_FILE).ml
	cp src/$(ARCH_FILE).ml src/arch.ml

src/case_ins.ml : src/$(CASE_FILE).ml
	cp src/$(CASE_FILE).ml src/case_ins.ml

src/tparser.ml : src/tparser.in src/trealparserin.in
	cp src/tparser.in src/tparser.ml

src/arch2.ml :
	echo "let associate_these a = Var.set_string \"WEIDU_ARCH\" \"$(WEIDU_ARCH)\" ; Var.set_string \"WEIDU_OS\" \"$(WEIDU_OS)\"; Var.set_string \"WEIDU_VER\" !Util.weidu_version" > src/arch2.ml
	echo "let _ = associate_these ()" >> src/arch2.ml

$(OBJDIR)/tparser.cmx : src/trealparserin.in src/tlexer.in src/make_gr.ml src/aliases.in src/make_tll.ml

# Include now the common set of rules for OCAML
include Makefile.msvc
include Makefile.ocaml
CAMLFLAGS      += -I zlib -I xdiff

   # Now the rule to make WeiDU

PROJECT_EXECUTABLE = $(OBJDIR)/weidu$(EXE)
PROJECT_MODULES    = $(WEIDU_MODULES)
ifdef HAVE_MSVC
PROJECT_CMODULES   = reg
PROJECT_CLIBS      = C:\\Windows\\System32\\advapi32.dll
endif
ifdef HAVE_MINGW
PROJECT_CMODULES   = reg
PROJECT_CLIBS      = -ladvapi32
endif
#PROJECT_CMODULES   += eff_strings
PROJECT_CMODULES   += zlib adler32 inflate uncompr inftrees zutil inffast $(GLOB) xdiff
PROJECT_CMODULES   += xemit xpatchi xutils xdiffi xprepare $(ARCH_C_FILES)
PROJECT_CMODULES   += crc32 compress deflate trees

PROJECT_LIBS       = unix str
.PHONY: weidu
weidu:  $(PROJECT_EXECUTABLE)
$(PROJECT_EXECUTABLE) : $(PROJECT_MODULES:%=$(OBJDIR)/%.$(CMO)) \
                        $(PROJECT_CMODULES:%=$(OBJDIR)/%.$(OBJEXT))
	@$(NARRATIVE) Linking $(COMPILETOWHAT) $@
	$(CAMLLINK) -o $@ \
                    $(PROJECT_LIBS:%=%.$(CMXA)) \
                    $(PROJECT_LIBS:%=-cclib -l%) \
                    $(PROJECT_CLIBS:%=-cclib %) \
                    $^
	cp $(PROJECT_EXECUTABLE) .

# rule for Weimorph

PROJECT2_EXECUTABLE = $(OBJDIR)/weimorph$(EXE)
PROJECT2_MODULES    = myarg $(WEIDU_BASE_MODULES) \
                        iwgconf iwgrule iwgparser iwglexer iwg2
ifdef HAVE_MSVC
PROJECT2_CMODULES   = reg
PROJECT2_CLIBS      = advapi32.lib
endif
ifdef HAVE_MINGW
PROJECT2_CMODULES   = reg
PROJECT2_CLIBS      = -ladvapi32
endif
#PROJECT2_CMODULES   += eff_strings
PROJECT_CMODULES   += zlib adler32 inflate uncompr inftrees zutil inffast $(GLOB) xdiff
PROJECT_CMODULES   += xemit xpatchi xutils xdiffi xprepare $(ARCH_C_FILES)
PROJECT_CMODULES   += crc32 compress deflate trees
PROJECT2_LIBS       = unix str
.PHONY: weimorph
weimorph: $(PROJECT2_EXECUTABLE)
$(PROJECT2_EXECUTABLE) : $(PROJECT2_MODULES:%=$(OBJDIR)/%.$(CMO)) \
                        $(PROJECT2_CMODULES:%=$(OBJDIR)/%.$(OBJEXT))
	@$(NARRATIVE) Linking $(COMPILETOWHAT) $@
	$(CAMLLINK) -o $@ \
                    $(PROJECT2_LIBS:%=%.$(CMXA)) \
                    $(PROJECT2_LIBS:%=-cclib -l%) \
                    $(PROJECT2_CLIBS:%=-cclib %) \
                    $^
	cp $(PROJECT2_EXECUTABLE) .

# compile WeiGUI with the Labltk bindings!
weigui: FORCE
	$(MAKE) -f Makefile-tk weigui

# rule for tolower

PROJECT3_EXECUTABLE = $(OBJDIR)/tolower$(EXE)
PROJECT3_MODULES    = tolower
PROJECT3_CMODULES   =
PROJECT3_LIBS       = unix str
.PHONY: tolower
tolower: $(PROJECT3_EXECUTABLE)
$(PROJECT3_EXECUTABLE) : $(PROJECT3_MODULES:%=$(OBJDIR)/%.$(CMO)) \
                        $(PROJECT3_CMODULES:%=$(OBJDIR)/%.$(OBJEXT))
	@$(NARRATIVE) Linking $(COMPILETOWHAT) $@
	$(CAMLLINK) -o $@ \
                    $(PROJECT3_LIBS:%=%.$(CMXA)) \
                    $(PROJECT3_LIBS:%=-cclib -l%) \
                    $(PROJECT3_CLIBS:%=-cclib %) \
                    $^
	cp $(PROJECT3_EXECUTABLE) .

# rule for WeInstall

PROJECT4_EXECUTABLE = $(OBJDIR)/weinstall$(EXE)
PROJECT4_MODULES    =  case_ins weinstall
PROJECT4_CMODULES   =
PROJECT4_LIBS       = unix str
.PHONY: weinstall
weinstall: $(PROJECT4_EXECUTABLE)
$(PROJECT4_EXECUTABLE) : $(PROJECT4_MODULES:%=$(OBJDIR)/%.$(CMO)) \
                        $(PROJECT4_CMODULES:%=$(OBJDIR)/%.$(OBJEXT))
	@$(NARRATIVE) Linking $(COMPILETOWHAT) $@
	$(CAMLLINK) -o $@ \
                    $(PROJECT4_LIBS:%=%.$(CMXA)) \
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
	src/tlexer.mll src/trealparserin.gr $(OBJDIR)/mytop*  \
	src/toldlexer.mll src/tph.ml
	find obj -exec rm {} \;
	$(MAKE) -f Makefile-tk clean


###
### Distro
###
VER = $(shell grep "let version" src/version.ml | cut -d \" -f 2 | sed -e's/\(...\)../\1/g')
IWG2 = `grep "let iwg2_version" /cygdrive/c/src/WeiDU/src/iwg2.ml | cut -d ' ' -f 4`
VERBIG = $(shell grep "let version" src/version.ml | cut -d \" -f 2)
doc: doc/base.tex
	$(MAKE) -C doc
	cat README.html | sed -e "s/&ndash;/--/g" -e's/&#X2013;/--/g' > README-WeiDU.html
	rm README.html

ifdef ITEMLIST
zip :
	echo cannot make zip -- itemlist included
linux_zip :
  echo cannot make zip -- itemlist included
else
windows_zip : weidu weinstall weigui tolower clean
	rm iwg2* weimorph* || true
	mv weid*.exe WeiDU.exe
	mv weig*.exe WeiGUI.exe
	mv wein*.exe WeInstall.exe
	mv tolo*.exe ToLower.exe
	strip WeiDU.exe || true
	upx --best WeiDU.exe || echo "No EXE Compression"
	strip WeiGUI.exe || true
	upx --best WeiGUI.exe || echo "No EXE Compression"
	strip WeInstall.exe || true
	upx --best WeInstall.exe || echo "No EXE Compression"
	strip ToLower.exe || true
	upx --best ToLower.exe || echo "No EXE Compression"
	(cd .. ; zip -9r WeiDU-Windows-$(VER).zip WeiDU/*.exe WeiDU/COPYING WeiDU/README* WeiDU/*.dll WeiDU/examples )
src_zip : clean
	(cd .. ; zip -9r WeiDU-Src-$(VER).zip WeiDU/* -x WeiDU/WeiDU.exe -x WeiDU/ToLower.exe \
   -x WeiDU/WeInstall.exe -x WeiDU/WeiGUI.exe -x WeiDU/*.dll; )
build : weidu
	rm iwg2* weimorph* || true
	cp weid*$(EXE) ../WeiDU-Linux/WeiDU || true
	cp wein*$(EXE) ../WeiDU-Linux/WeInstall || true
	cp weig*$(EXE) ../WeiDU-Linux/WeiGUI || true
	cp tolower$(EXE) ../WeiDU-Linux/tolower || true
linux_zip : weidu weinstall weigui tolower
	rm iwg2* weimorph* || true
	mv weid*$(EXE) ../WeiDU-Linux/WeiDU || true
	mv wein*$(EXE) ../WeiDU-Linux/WeInstall || true
	mv weig*$(EXE) ../WeiDU-Linux/WeiGUI || true
	mv tolower$(EXE) ../WeiDU-Linux/tolower || true
	strip ../WeiDU-Linux/WeiDU || true
	upx --best ../WeiDU-Linux/WeiDU || echo "No EXE Compression"
	strip ../WeiDU-Linux/WeInstall || true
	upx --best ../WeiDU-Linux/WeInstall || echo "No EXE Compression"
	strip ../WeiDU-Linux/WeiGUI || true
	upx --best ../WeiDU-Linux/WeiGUI || echo "No EXE Compression"
	strip ../WeiDU-Linux/tolower || true
	upx --best ../WeiDU-Linux/tolower || echo "No EXE Compression"
	cp README* ../WeiDU-Linux
	(cd .. ; zip -9r WeiDU-Linux-$(VER).zip WeiDU-Linux )
osx_zip : weidu weinstall weigui
	rm iwg2* weimorph* || true
	mv weid*$(EXE) ../WeiDU-mac/WeiDU/weidu-mac || true
	mv wein*$(EXE) ../WeiDU-mac/WeiDU/weinstall || true
	mv weig*$(EXE) ../WeiDU-mac/WeiDU/weigui    || true
	strip ../WeiDU-mac/WeiDU/weidu-mac || true
	strip ../WeiDU-mac/WeiDU/weinstall || true
	strip ../WeiDU-mac/WeiDU/weigui    || true
	cp README* ../WeiDU-mac/WeiDU
	sed -e's/version_plist=.*/version_plist=\"${VERBIG}\"/g'  '../WeiDU-mac/WeiDU/WeiDU Installer.command' > t
	mv t ../WeiDU-mac/WeiDU/WeiDU\ Installer.command
	(cd ../WeiDU-mac ; zip -9r ../WeiDU-Mac-$(VER).zip * )
endif

IWD2_DIR = "/cygdrive/c/Program Files/Black Isle/Icewind Dale II/"
IWD1_DIR = "/cygdrive/c/Program Files/Black Isle/BGII - SoA/"

iwg2zip: weimorph
	upx --best weimorph.asm$(EXE) || echo "Fine"
	cp weimorph.asm$(EXE) $(IWD2_DIR)/iwg2$(EXE)
	( cd $(IWD2_DIR) ; rm iwg2/errors/* || echo fine)
	( cd $(IWD2_DIR) ; /home/weimer/bin/rar a -s -m5 -sfx -r IWG2-v$(IWG2)$(EXE) iwg2$(EXE) "iwg2/*" )
	( cd $(IWD2_DIR) ; chmod a+r IWG2-v$(IWG2)$(EXE) )

iwg1zip: weimorph
	upx --best weimorph.asm$(EXE) || echo "Fine"
	cp weimorph.asm$(EXE) $(IWD1_DIR)/iwg1$(EXE)
	( cd $(IWD1_DIR) ; rm iwg1/errors/* || echo fine)
	( cd $(IWD1_DIR) ; /home/weimer/bin/rar a -s -m5 -sfx -r IWG1-v$(IWG2)$(EXE) iwg1$(EXE) "iwg1/*" )
	( cd $(IWD1_DIR) ; chmod a+r IWG1-v$(IWG2)$(EXE) )

FORCE:
