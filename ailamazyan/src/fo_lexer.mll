{
open Lexing
open Fo_parser
open Verified
}

let blank = [' ' '\t']+
let newline = ['\r' '\n'] | "\r\n"
let num = ['0'-'9']+
let alpha = ['a'-'z' 'A'-'Z']
let alphanums = ['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
  | newline                                       { Lexing.new_line lexbuf; token lexbuf }
  | blank                                         { token lexbuf }
  | ","                                           { COM }
  | "."                                           { DOT }
  | "("                                           { LPA }
  | ")"                                           { RPA }
  | "="                                           { EQ }
  | "FALSE"                                       { FALSE }
  | "TRUE"                                        { TRUE }
  | "NOT"                                         { NEG }
  | "AND"                                         { CONJ }
  | "OR"                                          { DISJ }
  | "EXISTS"                                      { EXISTS }
  | "FORALL"                                      { FORALL }
  | "x" (num as n)                                { VAR (Eval_FO.nat_of_integer (Z.of_string n)) }
  | num as n                                      { NAT (Eval_FO.nat_of_integer (Z.of_string n)) }
  | (alpha alphanums) as name                     { PRED name }
  | eof                                           { EOF }
  | _ as c                                        { failwith "Illegal character in formula file." }
