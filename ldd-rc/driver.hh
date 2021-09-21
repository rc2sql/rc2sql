#ifndef DRIVER_HH
#define DRIVER_HH
#include <string>
#include "formula.hh"
#include "parser.hh"

// Give Flex the prototype of yylex we want ...
#define YY_DECL \
  yy::parser::symbol_type yylex (driver& drv)
// ... and declare it for the parser's sake.
YY_DECL;

// Conducting the whole scanning and parsing of Calc++.
class driver
{
public:
  Formula *res;

  // Run the parser on file F.  Return 0 on success.
  int parse (const std::string& f)
  {
    file = f;
    location.initialize (&file);
    scan_begin ();
    yy::parser parse (*this);
    //parse.set_debug_level (trace_parsing);
    int res = parse ();
    scan_end ();
    return res;
  }
  // The name of the file being parsed.
  std::string file;

  // Handling the scanner.
  void scan_begin ();
  void scan_end ();
  // The token's location used by the scanner.
  yy::location location;
};

#endif
