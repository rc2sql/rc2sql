open Trans

let _ =
  let prefix = Sys.argv.(1) in
  let (f, tdb(*, db*)) = parse prefix in
  let _ = vgtrans (prefix ^ ".v") tdb (*db *)f in
  let _ =
    (let ch = open_out (prefix ^ ".mfotl") in
     Printf.fprintf ch "%s\n" (mfotl_of_fmla f); close_out ch) in
  ()
