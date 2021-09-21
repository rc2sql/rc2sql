ddd: builder.cpp builder.hh db.hh driver.hh formula.hh lexer.hh lexer.cpp main.cpp parser.hh parser.cpp visitors.h
	g++ -O2 -fno-operator-names -I/home/rcsql/dddlib/c -I/home/rcsql/dddlib/include -L/home/rcsql/dddlib/lib -o ddd main.cpp builder.cpp lexer.cpp parser.cpp -ldddcpp -lddd

lexer.hh: lexer.ll
	flex lexer.ll

lexer.cpp: lexer.ll
	flex lexer.ll

parser.hh: parser.yy
	bison parser.yy

parser.cpp: parser.yy
	bison parser.yy
