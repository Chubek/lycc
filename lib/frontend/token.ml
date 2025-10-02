type t =
  (* Keywords *)
  | Auto | Break | Case | Char | Const | Continue | Default | Do
  | Double | Else | Enum | Extern | Float | For | Goto | If
  | Inline | Int | Long | Register | Return | Short | Signed
  | Sizeof | Static | Struct | Switch | Typedef | Union | Unsigned
  | Void | Volatile | While

  (* Identifiers and constants *)
  | Identifier of string
  | IntegerConstant of int64 * int_suffix
  | FloatingConstant of float * float_suffix
  | CharacterConstant of char
  | StringLiteral of string

  (* Operators *)
  | Plus | Minus | Star | Slash | Percent
  | Ampersand | Pipe | Caret | Tilde | Exclamation
  | Less | Greater | LessEqual | GreaterEqual
  | EqualEqual | NotEqual
  | LeftShift | RightShift
  | LogicalAnd | LogicalOr
  | Equal | PlusEqual | MinusEqual | StarEqual | SlashEqual
  | PercentEqual | AmpersandEqual | PipeEqual | CaretEqual
  | LeftShiftEqual | RightShiftEqual
  | Increment | Decrement
  | Arrow | Dot

  (* Punctuation *)
  | LeftParen | RightParen
  | LeftBracket | RightBracket
  | LeftBrace | RightBrace
  | Semicolon | Comma | Colon | Question

  (* Special *)
  | Eof
[@@deriving show, eq, sexp]

and int_suffix = NoSuffix | U | L | UL | LL | ULL
and float_suffix = NoFloatSuffix | F | L_suffix
