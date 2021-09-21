%{
# include <cerrno>
# include <climits>
# include <cstdlib>
# include <cstring>
# include <string>
# include "driver.hh"
# include "parser.hh"
%}

%option noyywrap nounput noinput batch
%option outfile="lexer.cpp" header-file="lexer.hh"

%{
  // A number symbol corresponding to the value in S.
  yy::parser::symbol_type make_NUMBER(const std::string &s, const yy::parser::location_type &loc);
%}

LPAREN      \(
RPAREN      \)
EQ          =
id          [a-zA-Z][a-zA-Z_0-9]*
int         [0-9]+
blank       [ \t\r\n]

%{
  // Code run each time a pattern is matched.
  # define YY_USER_ACTION loc.columns(yyleng);
%}
%%
%{
  // A handy shortcut to the location held by the driver.
  yy::location &loc = drv.location;
  // Code run each time yylex is called.
  loc.step();
%}

{blank}+            loc.step();
,                   { return yy::parser::make_COM(loc); }
\.                  { return yy::parser::make_DOT(loc); }
{LPAREN}            { return yy::parser::make_LPA(loc); }
{RPAREN}            { return yy::parser::make_RPA(loc); }
{EQ}                { return yy::parser::make_EQ(loc); }
NOT                 { return yy::parser::make_NEG(loc); }
AND                 { return yy::parser::make_CONJ(loc); }
OR                  { return yy::parser::make_DISJ(loc); }
EXISTS              { return yy::parser::make_EXISTS(loc); }
FORALL              { return yy::parser::make_FORALL(loc); }
{id}                { return yy::parser::make_IDENTIFIER (yytext, loc); }
{int}               { return make_NUMBER (yytext, loc); }
.                   { throw yy::parser::syntax_error(loc, "invalid character: " + std::string(yytext)); }
<<EOF>>             return yy::parser::make_END(loc);
%%

yy::parser::symbol_type make_NUMBER(const std::string &s, const yy::parser::location_type &loc)
{
  errno = 0;
  long n = strtol (s.c_str(), NULL, 10);
  if (! (INT_MIN <= n && n <= INT_MAX && errno != ERANGE))
    throw yy::parser::syntax_error (loc, "integer is out of range: " + s);
  return yy::parser::make_NUMBER ((int) n, loc);
}

void driver::scan_begin()
{
  if (file.empty () || file == "-")
    yyin = stdin;
  else if (!(yyin = fopen (file.c_str (), "r")))
    {
      std::cerr << "cannot open " << file << ": " << strerror(errno) << '\n';
      exit (EXIT_FAILURE);
    }
}

void driver::scan_end()
{
  fclose (yyin);
}
