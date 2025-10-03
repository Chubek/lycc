type directive =
  | Include of include_type * string
  | Define of string * macro_def
  | Undef of string
  | If of pp_expr
  | Ifdef of string
  | Ifndef of string  
  | Elif of pp_expr
  | Else
  | Endif
  | Line of int * string option
  | Error of string
  | Pragma of string list
  | Empty

and include_type = System | User

and macro_def = {
  params : string list option;  
  variadic : bool;
  replacement : pp_token list;
  defined_at : source_loc;
}

and pp_expr =
  | PPConstant of int64
  | PPDefined of string
  | PPNot of pp_expr
  | PPAnd of pp_expr * pp_expr
  | PPOr of pp_expr * pp_expr
  | PPBinary of pp_binop * pp_expr * pp_expr
  | PPTernary of pp_expr * pp_expr * pp_expr
  | PPParen of pp_expr

let parse_directive tokens =
  match tokens with
  | PPDirective "include" :: rest ->
    parse_include rest

  | PPDirective "define" :: PPIdentifier name :: rest ->
    parse_define name rest

  | PPDirective "undef" :: PPIdentifier name :: _ ->
    Ok (Undef name)

  | PPDirective "if" :: rest ->
    let* expr = parse_pp_expression rest in
    Ok (If expr)

  | PPDirective "ifdef" :: PPIdentifier name :: _ ->
    Ok (Ifdef name)

  | PPDirective "ifndef" :: PPIdentifier name :: _ ->
    Ok (Ifndef name)

  | PPDirective "elif" :: rest ->
    let* expr = parse_pp_expression rest in
    Ok (Elif expr)

  | PPDirective "else" :: _ ->
    Ok Else

  | PPDirective "endif" :: _ ->
    Ok Endif

  | PPDirective "error" :: rest ->
    Ok (Error (stringify_tokens rest))

  | PPDirective "pragma" :: rest ->
    Ok (Pragma (tokenize_pragma rest))

  | PPDirective dir :: _ ->
    Error (`Unknown_directive dir)

  | _ -> Ok Empty
