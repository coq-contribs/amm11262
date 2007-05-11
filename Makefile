##############################################################################
##                 The Calculus of Inductive Constructions                  ##
##                                                                          ##
##                                Projet Coq                                ##
##                                                                          ##
##                     INRIA                        ENS-CNRS                ##
##              Rocquencourt                        Lyon                    ##
##                                                                          ##
##                                  Coq V7                                  ##
##                                                                          ##
##                                                                          ##
##############################################################################

# WARNING
#
# This Makefile has been automagically generated by coq_makefile
# Edit at your own risks !
#
# END OF WARNING

#
# This Makefile was generated by the command line :
# coq_makefile -o Makefile ascii_format/ unicode_format/ 
#

##########################
#                        #
# Variables definitions. #
#                        #
##########################

CAMLP4LIB=`camlp4 -where`
COQSRC=-I $(COQTOP)/kernel -I $(COQTOP)/lib \
  -I $(COQTOP)/library -I $(COQTOP)/parsing \
  -I $(COQTOP)/pretyping -I $(COQTOP)/interp \
  -I $(COQTOP)/proofs -I $(COQTOP)/syntax -I $(COQTOP)/tactics \
  -I $(COQTOP)/toplevel -I $(COQTOP)/contrib/correctness \
  -I $(COQTOP)/contrib/extraction -I $(COQTOP)/contrib/field \
  -I $(COQTOP)/contrib/fourier -I $(COQTOP)/contrib/graphs \
  -I $(COQTOP)/contrib/interface -I $(COQTOP)/contrib/jprover \
  -I $(COQTOP)/contrib/omega -I $(COQTOP)/contrib/romega \
  -I $(COQTOP)/contrib/ring -I $(COQTOP)/contrib/xml \
  -I $(CAMLP4LIB)
ZFLAGS=$(OCAMLLIBS) $(COQSRC)
OPT=
COQFLAGS=-q $(OPT) $(COQLIBS) $(OTHERFLAGS) $(COQ_XML)
COQC=$(COQBIN)coqc
GALLINA=gallina
COQDOC=$(COQBIN)coqdoc
CAMLC=ocamlc -c
CAMLOPTC=ocamlopt -c
CAMLLINK=ocamlc
CAMLOPTLINK=ocamlopt
COQDEP=$(COQBIN)coqdep -c
GRAMMARS=grammar.cma
CAMLP4EXTEND=pa_extend.cmo pa_ifdef.cmo q_MLast.cmo
PP=-pp "camlp4o -I . -I $(COQTOP)/parsing $(CAMLP4EXTEND) $(GRAMMARS) -impl"

#########################
#                       #
# Libraries definition. #
#                       #
#########################

OCAMLLIBS=-I .
COQLIBS=-I .

###################################
#                                 #
# Definition of the "all" target. #
#                                 #
###################################

VFILES=
VOFILES=$(VFILES:.v=.vo)
VIFILES=$(VFILES:.v=.vi)
GFILES=$(VFILES:.v=.g)
HTMLFILES=$(VFILES:.v=.html)
GHTMLFILES=$(VFILES:.v=.g.html)

all: ascii_format/\
  unicode_format/

###################
#                 #
# Subdirectories. #
#                 #
###################

ascii_format/:
	cd ascii_format/ ; $(MAKE) all

unicode_format/:
	cd unicode_format/ ; $(MAKE) all

####################
#                  #
# Special targets. #
#                  #
####################

.PHONY: all opt byte archclean clean install depend html ascii_format/ unicode_format/

byte:
	$(MAKE) all "OPT="

opt:
	$(MAKE) all "OPT=-opt"

.depend depend:
	(cd ascii_format/ ; $(MAKE) depend)
	(cd unicode_format/ ; $(MAKE) depend)

install:
	mkdir -p `$(COQC) -where`/user-contrib
	(cd ascii_format/ ; $(MAKE) install)
	(cd unicode_format/ ; $(MAKE) install)

clean:
	rm -f *.cmo *.cmi *.cmx *.o $(VOFILES) $(VIFILES) $(GFILES) *~
	rm -f all.ps all-gal.ps $(HTMLFILES) $(GHTMLFILES)
	(cd ascii_format/ ; $(MAKE) clean)
	(cd unicode_format/ ; $(MAKE) clean)

archclean:
	rm -f *.cmx *.o
	(cd ascii_format/ ; $(MAKE) archclean)
	(cd unicode_format/ ; $(MAKE) archclean)

html:
	(cd ascii_format/ ; $(MAKE) html)
	(cd unicode_format/ ; $(MAKE) html)

# WARNING
#
# This Makefile has been automagically generated by coq_makefile
# Edit at your own risks !
#
# END OF WARNING

