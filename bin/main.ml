module Value = struct
  type t =
    | Int of int
    | Float of float
    | String of string
    | Symbol of string
    | Nil
    | Quote of t list

  let rec print = function
    | Int x -> print_int x
    | Float x -> print_float x
    | String x -> print_string ("\"" ^ x ^ "\"")
    | Symbol x -> print_string x
    | Nil -> print_string "NIL"
    | Quote vals ->
        print_string "'(";
        List.iter
          (fun x ->
            print x;
            print_string ", ")
          vals;
        print_string ")"
end

exception Undefined

let evaluate text =
  let open Value in
  let sexp = Parsexp.Single.parse_string_exn text in
  let rec evaluate_sexp sexp =
    match sexp with
    | Sexplib0.Sexp.Atom s -> (
        try Int (int_of_string s)
        with _ -> (
          try Float (float_of_string s)
          with _ ->
            if
              Base.String.is_prefix s ~prefix:"'"
              && Base.String.is_suffix s ~suffix:"'"
            then String (Base.String.strip ~drop:(fun c -> c = '\'') s)
            else Symbol s ) )
    | Sexplib0.Sexp.List atoms -> (
        match atoms with
        | [] -> Nil
        | op :: vars -> (
            match op with
            | Sexplib0.Sexp.Atom "quote" -> Quote (List.map evaluate_sexp vars)
            | _ -> raise Undefined ) )
  in
  evaluate_sexp sexp

let main () =
  let text = input_line stdin in
  let value = evaluate text in
  Value.print value;
  print_newline ()

let () = main ()
