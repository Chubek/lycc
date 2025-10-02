type basic_type =
  | Void
  | Bool
  | Char | SignedChar | UnsignedChar
  | Short | UnsignedShort
  | Int | UnsignedInt
  | Long | UnsignedLong
  | LongLong | UnsignedLongLong
  | Float | Double | LongDouble
[@@deriving show, eq, sexp]

type type_qualifier = Const | Volatile | Restrict
[@@deriving show, eq, sexp]

type storage_class = 
  | Auto | Register | Static | Extern | Typedef
[@@deriving show, eq, sexp]

type t =
  | Basic of basic_type
  | Pointer of t
  | Array of t * int64 option  (* None for unsized arrays *)
  | Function of t * param list * bool  (* return type, params, variadic *)
  | Struct of string * field list option  (* None for incomplete types *)
  | Union of string * field list option
  | Enum of string * enumerator list option
  | Typedef of string * t
  | Qualified of t * type_qualifier list
[@@deriving show, eq, sexp]

and param = {
  param_type : t;
  param_name : string option;
}

and field = {
  field_type : t;
  field_name : string;
  field_offset : int64 option;  (* Computed during layout *)
  field_bit_width : int option;  (* For bit fields *)
}

and enumerator = {
  enum_name : string;
  enum_value : int64;
}
