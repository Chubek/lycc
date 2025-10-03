let process_line_continuations tokens =
  let rec process acc = function
    | [] -> Ok (List.rev acc)
    | PPOther '\\' :: PPNewline :: rest ->
      process acc rest
    | PPOther '\\' :: PPWhitespace ws :: PPNewline :: rest 
      when String.for_all is_horizontal_space ws ->
      Logs.warn (fun m -> m "whitespace after line continuation");
      process acc rest
    | tok :: rest ->
      process (tok :: acc) rest
  in
  process [] tokens
