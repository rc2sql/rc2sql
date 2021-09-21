%token <string> PRED
%token <Verified.Eval_FO.nat> VAR
%token <Verified.Eval_FO.nat> NAT
%token LPA RPA COM DOT EQ
%token FALSE TRUE NEG CONJ DISJ
%token EXISTS FORALL
%token EOF

%right EXISTS FORALL
%left DISJ
%left CONJ
%nonassoc NEG
%nonassoc DOT

%type <(Verified.Eval_FO.nat, string) Verified.Eval_FO.fo_fmla> formula
%start formula

%%

formula:
  | f=f EOF { f }

f:
  | LPA f=f RPA                           { f }
  | FALSE                                 { Verified.Eval_FO.Bool false }
  | TRUE                                  { Verified.Eval_FO.Bool true }
  | PRED LPA termlist RPA                 { Verified.Eval_FO.Pred ($1, $3) }
  | term EQ term                          { Verified.Eval_FO.Eqa ($1, $3) }
  | NEG f=f                               { Verified.Eval_FO.Neg f }
  | f=f CONJ g=f                          { Verified.Eval_FO.Conj (f, g) }
  | f=f DISJ g=f                          { Verified.Eval_FO.Disj (f, g) }
  | EXISTS VAR DOT f=f %prec EXISTS       { Verified.Eval_FO.Exists ($2, f) }
  | FORALL VAR DOT f=f %prec FORALL       { Verified.Eval_FO.Forall ($2, f) }

termlist:
  | term COM termlist       { $1 :: $3 }
  | term                    { [$1] }
  |                         { [] }

term:
  | NAT                     { Verified.Eval_FO.Const $1 }
  | VAR                     { Verified.Eval_FO.Var $1 }
