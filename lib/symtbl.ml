module Symtbl = struct
  module Identifier = struct
    type t =
      { uid: sym_id
      ; value: string
      ; kind: sym_kind
      }

    and sym_kind =
      | StringConst
      | VariableName
      | FunctionName
      | LabelName
      | TypeAlias
      | TypeTag

    and sym_id = int

    let compare s1 s2 = Int.compare s1.uid s2.uid
    let hash s = Hashtbl.hash s.uid
    let equal s1 s2 = (=) s1.uid s2.uid
  end

  module IdentifierMap = Map.Make(Identifier)

  type t = scope Stack.t

  and scope = 
    { map: symbol_info IdentifierMap.t
    ; level: scope_level
    }

  and symbol_info =
    { stgcls: storage_class
    ; value: ASTNode.t
    }

  and scope_level =
    | TransUnit
    | FunctionLevel
    | Block

  and storage_class =
    | ExternSTG
    | StaticSTG
    | AutoSTG
    | RegisterSTG
    | ThreadLocalSTG
end
