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
