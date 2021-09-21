####################################################################
#
#      Makefile for the core DDD library
#
#  (c) Copyright 1998-2002 Jesper Moller <jm@configit-software.com>
#      and Jakob Lichtenberg <jl@configit-software.com>
#      ConfigIt Software A/S, DK-2100 Copenhagen, Denmark.
#      ALL RIGHTS RESERVED
#
####################################################################

#######################  Directories and files #####################

# The filenames of the core C header file and libraries
DDDHEADER=	ddd.h
DDDLIBDYN=	libddd.so
DDDLIBSTAT=	libddd.a
DDDINCDIR=	../include
DDDLIBDIR=	../lib

#######################  Files  #####################################

OBJECTS=types.o variable.o functions.o table.o io.o init.o \
	memory.o cache.o constraint-system.o primes.o matrix.o

# The header files from which exports are copied to the ddd header file
# The order is significant
HEADERS=types.h variable.h functions.h table.h io.h init.h 

###########################   Targets    #############################

all: $(DDDHEADER) $(DDDLIBDYN) $(DDDLIBSTAT)

install: all $(DDDINCDIR) $(DDDLIBDIR)
	@cp $(DDDHEADER) $(DDDINCDIR)
	@cp $(DDDLIBDYN) $(DDDLIBDIR)
	@cp $(DDDLIBSTAT) $(DDDLIBDIR)
	@echo "Done making install."

uninstall:
	@rm -f $(DDDINCDIR)/$(DDDHEADER) \
	$(DDDLIBDIR)/$(DDDLIBDYN) $(DDDLIBDIR)/$(DDDLIBSTAT)
	@echo "Done making uninstall."

clean:
	@rm -f *.o *~ core makeheader{,.o} .depend \
	$(DDDHEADER) $(DDDLIBDYN) $(DDDLIBSTAT)
	@echo "Done making clean."

$(DDDLIBDYN): $(OBJECTS)
	@$(CC) -shared -Wl,-soname,$@ -o $@ $(OBJECTS)
	@echo "Done making dynamic link library $@."

$(DDDLIBSTAT): $(OBJECTS)
	@ar rcu $@ $(OBJECTS)
	@echo "Done making static link library $@."

$(DDDHEADER): makeheader $(HEADERS)
	@./makeheader DDD_H $(HEADERS) >$(DDDHEADER)
	@echo "Done making header file $@."

# The makeheader program is used to generate the header file:
makeheader: makeheader.o
	@$(CC) -o $@ $<
	@echo "Done making $@."


$(DDDINCDIR) $(DDDLIBDIR):
	@mkdir -p $@

##########################  Conversions   ########################

.PRECIOUS:	$(DDDLIBSTAT) $(DDDLIBDYN) $(DDDHEADER)

include ../Makefile.common

include .depend

.depend:
	@$(CC) $(CFLAGS) -MM -MG *.c >.depend
	@echo "Done making $@"
