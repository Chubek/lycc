module TyyLattice = struct
  module Type = struct
    type t =
      { uid: tyy_id
      ; base: tyy_base
      ; qual: tyy_qual option
      ; ptr: tyy_ptr list
      }

    and tyy_base =
      | Primitive of tyy_prim
      | Compound of tyy_comp
      | Enumerative of tyy_enum
      | Applicative of tyy_appl

    and tyy_prim =
      | Void
      | Byte of bool * bool
      | Half of bool * bool
      | Word of bool * bool
      | Double of bool * bool
      | Quadrupe of bool * bool
      | Float32
      | Float64
      | MachineFloat
      | Complex128

    and tyy_comp =
      | Array of { elt_tyy: tyy_id 
                 ; dims: (int option * bool * bool * bool * bool) list
                 ; vla: string list
                 }
      | LUT of { key_tyy: tyy_id
               ; val_tyy: tyy_id
               }
      | Sum of { tag_name: Symtbl.Symbol.t
               ; join_enum: tyy_id
               ; join_union: tyy_id
               ; misc_fields: tyy_id list
               }
      | Product of { tag_name: Symtbl.Symbol.t
                   ; fields: tyy_id list
                   }
      | Union of { tag_name: Symtbl.Symbol.t
                 ; fields: tyy_id list
                 }

    and tyy_enum =
      { tag_name: Symtbl.Symbol.t
      ; variants: (string * int) list
      }

    and tyy_appl =
      { returns: tyy_id
      ; formals: (string option * tyy_id) list
      }

    and tyy_qual =
      | Const
      | Volatile
      | Restrict
      | Atomic

    and tyy_ptr =
      | Immutable
      | Mutable

    and tyy_id = int

    let compare t1 t2 = Int.compare t1.uid t2.uid
    let hash t = Hashtbl.hash t.uid
    let equal t1 t2 = (=) t1.uid t2.uid
  end

end
