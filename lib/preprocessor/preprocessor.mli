module Preprocessor : sig
  type state
  type config = {
    include_paths : string list;
    system_paths : string list;
    predefined_macros : (string * macro_def) list;
    max_include_depth : int;  (* Default: 200 *)
    enable_warnings : bool;
  }
  
  type source_loc = {
    file : string;
    line : int;
    column : int;
    included_from : source_loc option;  (* Include stack *)
  }
  
  type pp_token = {
    token : Token.t;
    loc : source_loc;
    raw_text : string;  (* Preserved for stringification *)
    preceding_whitespace : bool;  (* For proper macro expansion *)
  }
  
  val create : config -> state
  val preprocess_file : state -> string -> (pp_token list, error) result
  val preprocess_tokens : state -> pp_token list -> (pp_token list, error) result
end
