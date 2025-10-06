module TyyLattice = struct
  module Type = struct
    type t =
      { uid: tyy_id
      ; base: tyy_base
      ; qual: tyy_qual
      ; indir: int
      }

    and tyy_base =
      | Primitive of tyy_prim
      | Composite of tyy_comp
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
      | Aggregate of { elt_tyy: tyy_id 
                     ; dims: (int option * bool * bool * bool * bool) list
                     ; vla: string list
                     }
      | Sum of { tag_name: string
               ; join_enum: tyy_id
               ; join_union: tyy_id
               ; misc_fields: tyy_id list
               }
      | Product of { tag_name: string
                   ; fields: tyy_id list
                   }
      | Union of { tag_name: string
                 ; fields: tyy_id list
                 }

    and tyy_enum =
      { tag_name: string
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

    and tyy_id = int
  end
end
