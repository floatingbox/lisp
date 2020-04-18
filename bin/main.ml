module Value = struct
  type t = Int of int | Float of float | String of string

  let print = function
    | Int x -> print_int x
    | Float x -> print_float x
    | String x -> print_string x
end

let evaluate text =
  let open Value in
  let sexp = Parsexp.Single.parse_string_exn text in
  match sexp with
  | Sexplib0.Sexp.Atom s -> (
      try Int (int_of_string s)
      with _ -> ( try Float (float_of_string s) with _ -> String s ) )
  | _ -> Int 0

let main () =
  let text = input_line stdin in
  let value = evaluate text in
  Value.print value;
  print_newline ()

let () = main ()
