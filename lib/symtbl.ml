module Symtbl = struct
  module Identifier = struct
    type t =
      { uid: ident_id
      ; value: string
      ; kind: ident_kind
      }

    and ident_kind =
      | StringConst
      | VariableName
      | FunctionName
      | LabelName
      | TypeAlias
      | DatatypeTag
      | FormalParam
      | ActualParam
      | EnumConst

    and ident_id = int

    let compare s1 s2 = Int.compare s1.uid s2.uid
    let hash s = Hashtbl.hash s.uid
    let equal s1 s2 = (=) s1.uid s2.uid

    let uid_counter = ref 0

    let issue_uid () =
      let uid = !uid_counter in
      incr uid_counter;
      uid
  end

  module IdentifierMap = Map.Make(Identifier)

  type t = scope Stack.t

  and scope = 
    { mutable map: symbol_info IdentifierMap.t
    ; level: scope_level
    }

  and symbol_info =
    { stgcls: storage_class option
    ; typ: TypeLattice.Type.t
    ; defined: bool
    ; value: Absyn.ASTNode.t
    ; loc: Absyn.Location.t
    }

  and scope_level =
    | FileScope of string
    | FunctionBody of Identifier.t
    | Block of int * Identifier.t

  and storage_class =
    | ExternSTG
    | StaticSTG
    | AutoSTG
    | RegisterSTG
    | ThreadLocalSTG
    | TypedefSTG

  let enter_file_scope stb filename =
    let scope = { map = IdentifierMap.empty ; level = FileScope filename } in
    Stack.push scope stb;
    stb

  let enter_function_scope stb funname =
    let top_scope = Stack.top stb in
    let scope = { map = top_scope.map ; level = FunctionBody funname } in
    Stack.push scope stb;
    stb

  let enter_block_scope stb =
    let top_scope = Stack.top stb in
    let (funname, nesting) = 
      match top_scope.level with
      | FunctionBody fnnm -> (fnnm, 1)
      | Block (n, fnm) -> (fnm, n)
      | _ -> failwith "Block scope must be parent of function scope"
    in
    let scope = { map = top_scope.map ; level = BlockScope (nesting, funname) } in
    Stack.push scope stb;
    stb
end
