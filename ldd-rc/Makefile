ldd: builder.cpp builder.hh db.hh driver.hh formula.hh lexer.hh lexer.cpp main.cpp parser.hh parser.cpp visitors.h
	g++ -O2 -o ldd -I ${LDDIR} -I ${LDDIR}/src/include -L ${LDDIR}/src/ldd -I ${LDDIR}/cudd-2.4.2/include -L ${LDDIR}/cudd-2.4.2/cudd -L ${LDDIR}/cudd-2.4.2/util -L ${LDDIR}/cudd-2.4.2/mtr -L ${LDDIR}/cudd-2.4.2/epd -L ${LDDIR}/cudd-2.4.2/st -I ${LDDIR}/src/tvpi -L ${LDDIR}/src/tvpi main.cpp builder.cpp lexer.cpp parser.cpp -ltvpi -lgmp -lldd -lcudd -lutil -lmtr -lepd -lst -DSIZEOF_VOID_P=8

lexer.hh: lexer.ll
	flex lexer.ll

lexer.cpp: lexer.ll
	flex lexer.ll

parser.hh: parser.yy
	bison parser.yy

parser.cpp: parser.yy
	bison parser.yy
