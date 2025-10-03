module Macro_table = struct
  type t = {
    macros : (string, macro_def) Hashtbl.t;
    expansion_stack : string list;  
  }

  let create () = {
    macros = Hashtbl.create 1024;
    expansion_stack = [];
  }

  let define t name def =
    if List.mem name t.expansion_stack then
      Error (`Recursive_macro name)
    else begin
      Hashtbl.replace t.macros name def;
      Ok ()
    end

  let undefine t name =
    Hashtbl.remove t.macros name

  let lookup t name =
    Hashtbl.find_opt t.macros name

  let is_defined t name =
    Hashtbl.mem t.macros name

  let add_predefined t =
    let predef = [
      "__LINE__", special_macro `Line;
      "__FILE__", special_macro `File;
      "__DATE__", special_macro `Date;
      "__TIME__", special_macro `Time;
      "__STDC__", object_macro [PPNumber "1"];
      "__STDC_VERSION__", object_macro [PPNumber "201112L"];
      "__STDC_HOSTED__", object_macro [PPNumber "1"];
    ] in
    List.iter (fun (name, def) -> 
        Hashtbl.add t.macros name def
      ) predef
end
