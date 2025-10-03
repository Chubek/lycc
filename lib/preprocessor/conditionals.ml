module Conditionals = struct
  type cond_state = {
    stack : cond_frame list;
    current_active : bool;
  }

  and cond_frame = {
    condition_met : bool;  
    in_else : bool;         
    active : bool;          
  }

  let process_conditionals state tokens =
    let rec process acc cond_state = function
      | [] when cond_state.stack <> [] ->
        Error (`Unterminated_conditional)

      | [] -> Ok (List.rev acc)

      | (tok :: rest) as tokens ->
        match extract_directive tok tokens with
        | Some (If expr, remaining) ->
          let active = cond_state.current_active && evaluate_expr state expr in
          let frame = { condition_met = active; in_else = false; active } in
          let new_state = {
            stack = frame :: cond_state.stack;
            current_active = cond_state.current_active && active;
          } in
          process acc new_state remaining

        | Some (Ifdef name, remaining) ->
          let defined = Macro_table.is_defined state.macros name in
          let active = cond_state.current_active && defined in
          let frame = { condition_met = active; in_else = false; active } in
          let new_state = {
            stack = frame :: cond_state.stack;
            current_active = cond_state.current_active && active;
          } in
          process acc new_state remaining

        | Some (Elif expr, remaining) when cond_state.stack <> [] ->
          let frame = List.hd cond_state.stack in
          if frame.in_else then
            Error (`Elif_after_else)
          else
            let active = 
              not frame.condition_met && 
              evaluate_expr state expr &&
              (match cond_state.stack with
               | [] -> true
               | _ :: outer -> 
                 List.fold_left (fun acc f -> acc && f.active) true outer)
            in
            let new_frame = { 
              frame with 
              condition_met = frame.condition_met || active;
              active 
            } in
            let new_state = {
              stack = new_frame :: List.tl cond_state.stack;
              current_active = active && 
                               (match List.tl cond_state.stack with
                                | [] -> true
                                | frames -> List.for_all (fun f -> f.active) frames);
            } in
            process acc new_state remaining

        | Some (Else, remaining) when cond_state.stack <> [] ->
          let frame = List.hd cond_state.stack in
          if frame.in_else then
            Error (`Multiple_else)
          else
            let active = not frame.condition_met in
            let new_frame = { 
              condition_met = true; 
              in_else = true; 
              active 
            } in
            let new_state = {
              stack = new_frame :: List.tl cond_state.stack;
              current_active = active &&
                               (match List.tl cond_state.stack with
                                | [] -> true
                                | frames -> List.for_all (fun f -> f.active) frames);
            } in
            process acc new_state remaining

        | Some (Endif, remaining) ->
          (match cond_state.stack with
           | [] -> Error (`Unmatched_endif)
           | _ :: rest ->
             let new_state = {
               stack = rest;
               current_active = 
                 List.for_all (fun f -> f.active) rest;
             } in
             process acc new_state remaining)

        | Some _ | None when cond_state.current_active ->
          process (tok :: acc) cond_state rest

        | _ ->
          process acc cond_state rest
    in
    process [] { stack = []; current_active = true } tokens
end
