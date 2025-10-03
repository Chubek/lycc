module Pipeline = struct
  type phase = 
    | Tokenization
    | LineSplicing
    | CommentRemoval
    | DirectiveProcessing
    | MacroExpansion
    | TokenConcatenation
    | Finalization
  
  let run_pipeline config source =
    let open Result.Let_syntax in
    let* tokens = Lexer.tokenize source in
    let* tokens = process_line_continuations tokens in
    let* tokens = remove_comments tokens in
    let* tokens = process_directives config tokens in
    let* tokens = expand_macros config tokens in
    let* tokens = concatenate_tokens tokens in
    finalize_tokens tokens
end
