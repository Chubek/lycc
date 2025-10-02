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

let sizeof = function
  | Basic Void -> Error "cannot take size of void"
  | Basic Bool -> Ok 1L
  | Basic (Char | SignedChar | UnsignedChar) -> Ok 1L
  | Basic (Short | UnsignedShort) -> Ok 2L
  | Basic (Int | UnsignedInt) -> Ok 4L
  | Basic (Long | UnsignedLong) -> Ok 8L
  | Basic (LongLong | UnsignedLongLong) -> Ok 8L
  | Basic Float -> Ok 4L
  | Basic Double -> Ok 8L
  | Basic LongDouble -> Ok 16L
  | Pointer _ -> Ok 8L
  | Array (elem_type, Some count) ->
      sizeof elem_type |> Result.map (Int64.mul count)
  | Array (_, None) -> Error "cannot take size of unsized array"
  | Struct (_, Some fields) -> compute_struct_size fields
  | Struct (_, None) -> Error "cannot take size of incomplete struct"
  | Union (_, Some fields) -> compute_union_size fields
  | Union (_, None) -> Error "cannot take size of incomplete union"
  | Enum _ -> Ok 4L  (* Enums are int-sized *)
  | Function _ -> Error "cannot take size of function"
  | Qualified (t, _) -> sizeof t
  | Typedef (_, t) -> sizeof t

let alignof = function
  | Basic Bool -> 1L
  | Basic (Char | SignedChar | UnsignedChar) -> 1L
  | Basic (Short | UnsignedShort) -> 2L
  | Basic (Int | UnsignedInt) -> 4L
  | Basic (Long | UnsignedLong) -> 8L
  | Basic (LongLong | UnsignedLongLong) -> 8L
  | Basic Float -> 4L
  | Basic Double -> 8L
  | Basic LongDouble -> 16L
  | Pointer _ -> 8L
  | Array (elem_type, _) -> alignof elem_type
  | Struct (_, Some fields) -> compute_struct_alignment fields
  | Union (_, Some fields) -> compute_union_alignment fields
  | Enum _ -> 4L
  | Qualified (t, _) -> alignof t
  | Typedef (_, t) -> alignof t
  | _ -> failwith "alignof: incomplete type"
