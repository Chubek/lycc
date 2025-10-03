module PP_Lexer = struct
  type pp_token_kind =
    | Regular of Token.t
    | PPDirective of string  
    | PPIdentifier of string  
    | PPNumber of string  
    | PPString of string * bool  
    | PPChar of string * bool  
    | PPHeader of string * bool  
    | PPConcat  
    | PPStringify  
    | PPNewline  
    | PPWhitespace of string
    | PPComment of string * bool  
    | PPOther of char

  let tokenize_for_pp input =
    let rec loop pos acc =
      if pos >= String.length input then
        List.rev acc
      else
        match peek_char input pos with
        | '/' when peek_ahead input (pos + 1) = Some '/' ->
          let comment, next_pos = scan_line_comment input pos in
          loop next_pos (PPComment (comment, false) :: acc)

        | '/' when peek_ahead input (pos + 1) = Some '*' ->
          let comment, next_pos = scan_block_comment input pos in
          loop next_pos (PPComment (comment, true) :: acc)

        | '#' when is_at_line_start acc ->
          let directive, next_pos = scan_directive input pos in
          loop next_pos (PPDirective directive :: acc)

        | '#' when peek_ahead input (pos + 1) = Some '#' ->
          loop (pos + 2) (PPConcat :: acc)

        | '#' -> loop (pos + 1) (PPStringify :: acc)

        | '<' when in_include_context acc ->
          let header, next_pos = scan_header_name input pos true in
          loop next_pos (PPHeader (header, true) :: acc)

        | '"' when in_include_context acc ->
          let header, next_pos = scan_header_name input pos false in
          loop next_pos (PPHeader (header, false) :: acc)

        | c when is_whitespace c ->
          let ws, next_pos = scan_whitespace input pos in
          loop next_pos (PPWhitespace ws :: acc)

        | '\n' ->
          loop (pos + 1) (PPNewline :: acc)

        | _ ->
          let tok, next_pos = scan_regular_token input pos in
          loop next_pos (tok :: acc)
    in
    loop 0 []
end
