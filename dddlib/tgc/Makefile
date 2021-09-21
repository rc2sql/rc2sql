####################################################################
#
#      Makefile for the C++ examples of the DDD library
#
#  (c) Copyright 1999 Jesper Moeller
#      The IT University in Copenhagen,
#      Glentevej 67, DK-2400 Copenhagen, Denmark.
#      jm@it.edu
#      ALL RIGHTS RESERVED
#
####################################################################


#######################  Directories and files #####################

# The filenames of the C++ header file and libraries
DDDCPPHEADER=	dddcpp.h
DDDCPPLIBDYN=	libdddcpp.so
DDDCPPLIBSTAT=	libdddcpp.a
DDDINCDIR=	../include
DDDLIBDIR=	../lib

#######################  Compiler options  #########################

CFLAGS += 	-I$(DDDINCDIR) -L$(DDDLIBDIR)

#######################  Files  #####################################

# The DDD libraries to link with the examples
LINKLIBS=	-ldddcpp -lddd

CPPFILES:=$(wildcard *.cpp)
OFILES  :=$(CPPFILES:.cpp=.o)

###########################   Targets    #############################

all: $(OFILES)


clean:
	rm -f *.o *~ core $(ALL_PRG)
	@echo "Done making clean."

.SUFFIXES:	.cpp .o

.o:
	$(CPP) $(CFLAGS) -o $@ $@.o $(LINKLIBS)

include ../Makefile.common
