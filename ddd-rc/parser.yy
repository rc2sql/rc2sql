%require "3.2"
%language "c++"
%defines

%output  "parser.cpp"
%defines "parser.hh"

%define api.token.constructor
%define api.value.type variant
%define parse.assert

%code requires {
  #include <string>
  #include "formula.hh"
  class driver;
}

// The parsing context.
%param { driver &drv }

%locations

%define parse.trace
%define parse.error verbose

%code {
  #include "driver.hh"
}

%define api.token.prefix {TOK_}
%token END 0 "end of file"
%token COM ","
%token DOT "."
%token LPA "("
%token RPA ")"
%token EQ "="
%token NEG "NOT"
%token CONJ "AND"
%token DISJ "OR"
%token EXISTS "EXISTS"
%token FORALL "FORALL"

%right "EXISTS" "FORALL"
%left "OR"
%left "AND"
%nonassoc "NOT"

%token <std::string> IDENTIFIER "identifier"
%token <int> NUMBER "number"
%type <Term *> trm
%type <std::vector<Term *> > trmlist
%type <Formula *> formula

%%
%start input;
 
input
    : formula { drv.res = $1; }
    ;

formula
    : "(" formula[f] ")" { $$ = $f; }
    | "identifier" "=" "identifier" { $$ = new EqFormula($1, $3); }
    | "identifier" "(" trmlist[trms] ")" { $$ = new PredFormula($1, $trms); }
    | "NOT" formula[f] { $$ = new NegFormula($f); }
    | formula[f] "AND" formula[g] { $$ = new AndFormula($f, $g); }
    | formula[f] "OR" formula[g] { $$ = new OrFormula($f, $g); }
    | "EXISTS" "identifier" "." formula[f] %prec "EXISTS" { $$ = new ExFormula($2, $f); }
    | "FORALL" "identifier" "." formula[f] %prec "FORALL" { $$ = new NegFormula(new ExFormula($2, new NegFormula($f))); }
    ;

trmlist
    : trm[t] { $$ = std::vector<Term *>(1, $t); }
    | trmlist[trms] "," trm[t] { $trms.push_back($t); $$ = $trms; }
    | { $$ = std::vector<Term *>(); }

trm
    : "number" { $$ = new CstTerm($1); }
    | "identifier"  { $$ = new VarTerm($1); }
    ;
%%

void yy::parser::error(const location_type& l, const std::string& m)
{
  std::cerr << l << ": " << m << '\n';
}
