####################################################################
#
#      Makefile for the C++ extension of the DDD library
#
#  (c) Copyright 1999 Jesper Moeller
#      The IT University in Copenhagen,
#      Glentevej 67, DK-2400 Copenhagen, Denmark.
#      jm@it.edu
#      ALL RIGHTS RESERVED
#
####################################################################

#######################  Directories and files #####################

# The name of the DDD C library
DDDLIB=	ddd

# The filenames of the C++ header file and libraries
DDDCPPHEADER=	dddcpp.h tgc.h
DDDCPPLIBDYN=	libdddcpp.so
DDDCPPLIBSTAT=	libdddcpp.a
DDDINCDIR=	../include
DDDLIBDIR=	../lib

#######################  Files  #####################################

DDDOBJECTS=	dddcpp.o tgc.o

CFLAGS = -I$(DDDINCDIR) -L$(DDDLIBDIR)

###########################   Targets    #############################

all: $(DDDCPPLIBDYN) $(DDDCPPLIBSTAT) $(STCPPLIBDYN) $(STCPPLIBSTAT)

install: $(DDDCPPHEADER) $(DDDCPPLIBDYN) $(DDDCPPLIBSTAT) $(DDDINCDIR) $(DDDLIBDIR)
	@cp $(DDDCPPHEADER) $(DDDINCDIR)
	@cp $(DDDCPPLIBDYN) $(DDDLIBDIR)
	@cp $(DDDCPPLIBSTAT) $(DDDLIBDIR)
	@echo "Done making install."

clean:
	rm -f *.o *~ core .depend $(DDDCPPLIBDYN) $(DDDCPPLIBSTAT)
	@echo "Done making clean."

$(DDDCPPLIBDYN): $(DDDOBJECTS)
	$(CPP) -shared -Wl,-soname,$@ -L$(DDDLIBDIR) -l$(DDDLIB) -o $@ $(DDDOBJECTS)
	@echo "Done making dynamic link library $@."

$(DDDCPPLIBSTAT): $(DDDOBJECTS)
	ar ruv $@ $(DDDOBJECTS)
	@echo "Done making static link library $@."

$(DDDINCDIR) $(DDDLIBDIR):
	@mkdir -p $@

##########################  Conversions   ########################

.PRECIOUS:	$(DDDCPPLIBSTAT) $(DDDCPPLIBDYN)

include ../Makefile.common




