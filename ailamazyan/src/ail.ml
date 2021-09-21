open Verified.Eval_FO

exception EXIT

let fmla_ref = ref None
let db_ref = ref None

let usage () = Format.printf
"Usage: ail -fmla test.fo -db test.db
Arguments:
  -fmla <file> - FO formula to be evaluated\n
  -db <file> - database file\n"; raise EXIT

let process_args =
  let rec go = function
    | ("-fmla" :: fmlafile :: args) ->
        fmla_ref := Some fmlafile;
        go args
    | ("-db" :: dbfile :: args) ->
        db_ref := Some dbfile;
        go args
    | [] -> ()
    | _ -> usage () in
  go

let string_of_list string_of_val sep left right xs =
  left ^ String.concat sep (List.map string_of_val xs) ^ right

let string_of_set_rbt rbt_fold string_of_val = function
  | RBT_set rbt -> string_of_list string_of_val "\n" "" ""
    (List.rev (rbt_fold (fun t ts -> t :: ts) rbt []))
  | _ -> "Unsupported set container!"

let string_of_nat n = Z.to_string (integer_of_nat n)

let string_of_sum string_of_Inl string_of_Inr = function
  | Inl x -> "Inl " ^ string_of_Inl x
  | Inr y -> "Inr " ^ string_of_Inr y

let _ =
    process_args (List.tl (Array.to_list Sys.argv));
    let f = match !fmla_ref with
      | None -> usage ()
      | Some fname ->
        let ch = open_in fname in
        let f = Fo_parser.formula Fo_lexer.token (Lexing.from_channel ch) in
        (close_in ch; f) in
    let db = match !db_ref with
      | None -> usage ()
      | Some fname ->
        let ch = open_in fname in
        let db = Db_parser.db Db_lexer.token (Lexing.from_channel ch) in
        (close_in ch; db) in
    let string_of_var n = "x" ^ string_of_nat n in
    let string_of_tuples rbt_fold string_of_val =
      string_of_set_rbt rbt_fold (string_of_list string_of_val "," "(" ")") in
    let conv_db = convert_nat_db db in
    match eval_fin_nat f conv_db with
      | Fin ts -> Printf.printf "Finite\n%s\n%s\n"
        (string_of_list (fun s -> s) "," "(" ")" (List.map (fun v -> "x" ^ string_of_nat v) (fv_fo_fmla_list f)))
        (string_of_tuples rbt_nat_list_fold string_of_nat ts)
      | Infin -> Printf.printf "Infinite\n"
      | _ -> Printf.printf "Unexpected error."
