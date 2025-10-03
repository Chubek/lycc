module Expansion = struct
  type expansion_context = {
    macros : Macro_table.t;
    current_file : string;
    current_line : int;
    disabled_macros : StringSet.t;  
  }

  let rec expand_tokens ctx tokens =
    match tokens with
    | [] -> []

    | { token = PPIdentifier name; _ } as tok :: rest
      when not (StringSet.mem name ctx.disabled_macros) ->
      (match Macro_table.lookup ctx.macros name with
       | Some macro_def -> 
         expand_macro ctx tok macro_def rest
       | None -> 
         tok :: expand_tokens ctx rest)

    | tok :: rest ->
      tok :: expand_tokens ctx rest

  and expand_macro ctx name_tok macro_def rest =
    match macro_def.params with
    | None ->

      let expanded = expand_replacement ctx macro_def.replacement [] in
      expanded @ expand_tokens ctx rest

    | Some params ->

      match rest with
      | { token = PPOther '('; _ } :: args ->
        let* arg_lists = parse_macro_args args in
        if List.length arg_lists <> List.length params &&
           not (macro_def.
                  variadic) 
        then Error (`Macro_arity_mismatch name_tok.token)
        else 
          let bindings = 
            List.combine params arg_lists @
            (if macro_def.variadic 
             then [("__VA_ARGS__", List.nth arg_lists (List.length params))]
             else [])
          in
          let expanded = expand_replacement ctx macro_def.replacement bindings in
          Ok (expanded @ expand_tokens ctx rest)
      | _ -> 

        Ok (name_tok :: expand_tokens ctx rest)

  and expand_replacement ctx repl bindings =
    let bind_map = StringMap.of_list bindings in
    let rec replace acc = function
      | [] -> List.rev acc
      | {token = PPIdentifier p; _} as tok :: t_rest ->
        (match StringMap.find_opt p bind_map with
         | Some arg_tokens -> replace (List.rev_append arg_tokens acc) t_rest
         | None -> replace (tok :: acc) t_rest)
      | {token = PPConcat; _} :: left :: right :: t_rest ->
        let pasted = paste_tokens left right in
        replace (pasted :: acc) t_rest
      | {token = PPStringify; _} :: {token = PPIdentifier p; raw_text; _} :: t_rest ->
        let stringified = Token.string_lit (stringify_raw bind_map p raw_text) in
        replace (stringified :: acc) t_rest
      | tok :: t_rest -> replace (tok :: acc) t_rest
    in replace [] repl

  and paste_tokens tok1 tok2 =
    let pasted = tok1.raw_text ^ tok2.raw_text in
    Token.from_string pasted tok1.loc

  and stringify_raw bind_map param raw_text =
    match StringMap.find_opt param bind_map with
    | Some tokens ->
      String.concat " " (List.map (fun t -> t.raw_text) tokens)
    | None -> raw_text
end
