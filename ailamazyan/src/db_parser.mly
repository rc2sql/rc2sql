%token LPA RPA COM
%token <Verified.Eval_FO.nat> CST
%token <string> PRED
%token EOF

%type <((string * Verified.Eval_FO.nat list) list)> db
%start db

%%

db:
  | PRED LPA fields RPA db { ($1, $3) :: $5 }
  | EOF                    { [] }

fields:
  | CST COM fields         { $1 :: $3 }
  | CST                    { [$1] }
  |                        { [] }
