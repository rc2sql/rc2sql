all: cmp gen_test

cmp: cmp.cpp
	g++ -o cmp -O2 cmp.cpp

gen_test: gen_test.cpp formula.h
	g++ -o gen_test -O2 gen_test.cpp
