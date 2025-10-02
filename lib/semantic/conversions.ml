(** Integer promotion rules *)
let promote_integer typ =
  match typ with
  | Types.Basic (Char | SignedChar | UnsignedChar | Short | UnsignedShort) ->
      Types.Basic Int
  | Types.Basic Bool -> Types.Basic Int
  | t -> t

(** Usual arithmetic conversions *)
let usual_arithmetic_conversions t1 t2 =
  match t1, t2 with
  | Types.Basic LongDouble, _ | _, Types.Basic LongDouble ->
      Types.Basic LongDouble
  | Types.Basic Double, _ | _, Types.Basic Double ->
      Types.Basic Double
  | Types.Basic Float, _ | _, Types.Basic Float ->
      Types.Basic Float
  | Types.Basic t1, Types.Basic t2 ->
      let t1' = promote_integer (Types.Basic t1) in
      let t2' = promote_integer (Types.Basic t2) in
      (* Apply integer conversion rank rules *)
      convert_integers t1' t2'
  | _ -> failwith "arithmetic conversion on non-arithmetic types"

(** Check if implicit conversion is allowed *)
let is_convertible ~from ~to_ =
  match from, to_ with
  (* Any arithmetic type converts to any other *)
  | Types.Basic b1, Types.Basic b2 when is_arithmetic b1 && is_arithmetic b2 ->
      true
  (* Array to pointer decay *)
  | Types.Array (elem, _), Types.Pointer target ->
      Types.equal elem target
  (* Function to function pointer *)
  | Types.Function _ as f, Types.Pointer target ->
      Types.equal f target
  (* Pointer conversions *)
  | Types.Pointer _, Types.Pointer (Types.Basic Void) -> true
  | Types.Pointer (Types.Basic Void), Types.Pointer _ -> true
  (* Null pointer constant *)
  | Types.Basic (Int | Long | LongLong), Types.Pointer _ 
    when is_null_constant from -> true
  (* Same type *)
  | t1, t2 when Types.equal t1 t2 -> true
  (* Otherwise no implicit conversion *)
  | _ -> false
