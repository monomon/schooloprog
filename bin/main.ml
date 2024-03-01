let args = match Sys.argv with
  | [||] -> ("../program.sexp", "cal.ics")
  | [|_; input_file; output_file|] -> (input_file, output_file)
  | _ -> failwith "Wrong args"

(*generate ics from sexp description*)
let () = let (input_file, output_file) = args in
  Schooloprog.sexp_to_ics_file input_file output_file
