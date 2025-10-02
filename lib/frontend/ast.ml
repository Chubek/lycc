type expr = {
  expr_desc : expr_desc;
  expr_type : Types.t option;  (* Filled by type checker *)
  expr_loc : Location.t;
}

and expr_desc =
  (* Primary expressions *)
  | Identifier of string
  | IntegerLiteral of int64
  | FloatingLiteral of float
  | StringLiteral of string
  | CharacterLiteral of char
  
  (* Postfix expressions *)
  | FunctionCall of expr * expr list
  | ArraySubscript of expr * expr
  | MemberAccess of expr * string
  | PointerMemberAccess of expr * string
  | PostIncrement of expr
  | PostDecrement of expr
  
  (* Unary expressions *)
  | PreIncrement of expr
  | PreDecrement of expr
  | UnaryOp of unary_op * expr
  | SizeofExpr of expr
  | SizeofType of Types.t
  | Cast of Types.t * expr
  
  (* Binary expressions *)
  | BinaryOp of expr * binary_op * expr
  
  (* Ternary conditional *)
  | Conditional of expr * expr * expr
  
  (* Assignment *)
  | Assignment of expr * assign_op * expr
  
  (* Comma *)
  | Comma of expr * expr

and unary_op =
  | Plus | Minus | Not | BitwiseNot | Deref | AddressOf

and binary_op =
  | Add | Sub | Mul | Div | Mod
  | Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual
  | BitwiseAnd | BitwiseOr | BitwiseXor
  | LeftShift | RightShift
  | LogicalAnd | LogicalOr

and assign_op =
  | Assign | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
  | AndAssign | OrAssign | XorAssign | LeftShiftAssign | RightShiftAssign
[@@deriving show, eq, sexp]

type stmt = {
  stmt_desc : stmt_desc;
  stmt_loc : Location.t;
}

and stmt_desc =
  | Expression of expr option  (* Expression statement or null statement *)
  | Block of block_item list
  | If of expr * stmt * stmt option
  | Switch of expr * stmt
  | While of expr * stmt
  | DoWhile of stmt * expr
  | For of for_init option * expr option * expr option * stmt
  | Goto of string
  | Continue
  | Break
  | Return of expr option
  | Label of string * stmt
  | Case of expr * stmt
  | Default of stmt

and for_init =
  | ForInitExpr of expr
  | ForInitDecl of declaration

and block_item =
  | Statement of stmt
  | Declaration of declaration
[@@deriving show, eq, sexp]

type declaration = {
  decl_specs : decl_spec list;
  init_declarators : init_declarator list;
  decl_loc : Location.t;
}

and decl_spec =
  | StorageClass of Types.storage_class
  | TypeSpec of type_spec
  | TypeQual of Types.type_qualifier
  | FunctionSpec of function_spec

and type_spec =
  | Void | Bool | Char | Short | Int | Long | Float | Double
  | Signed | Unsigned
  | StructOrUnion of struct_or_union
  | Enum of enum_spec
  | TypedefName of string

and function_spec = Inline | Noreturn

and init_declarator = {
  declarator : declarator;
  initializer : initializer option;
}

and declarator = {
  decl_name : string option;  (* None for abstract declarators *)
  decl_type : declarator_type;
}

and declarator_type =
  | Direct
  | Pointer of type_qualifier list * declarator_type
  | Array of declarator_type * array_size
  | Function of declarator_type * param_list

and array_size =
  | Unsized
  | Static of type_qualifier list * expr
  | Variable of type_qualifier list * expr
  | Fixed of expr option

and param_list =
  | Params of param_declaration list * bool  (* variadic flag *)
  | OldStyle of string list

and initializer =
  | Expression of expr
  | InitList of (designator list * initializer) list

and designator =
  | Index of expr
  | Field of string
[@@deriving show, eq, sexp]
