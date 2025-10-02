let rec check_expr ctx expr =
  let open Result.Let_syntax in
  match expr.expr_desc with
  | Identifier name ->
    let* typ = lookup_symbol ctx name in
    Ok { expr with expr_type = Some typ }

  | IntegerLiteral n ->
    let typ = infer_integer_type n in
    Ok { expr with expr_type = Some typ }

  | BinaryOp (e1, op, e2) ->
    let* e1' = check_expr ctx e1 in
    let* e2' = check_expr ctx e2 in
    let* result_type = check_binary_op op 
        (Option.get e1'.expr_type)
        (Option.get e2'.expr_type) in
    Ok { expr with 
         expr_desc = BinaryOp (e1', op, e2');
         expr_type = Some result_type }

  | Assignment (lhs, op, rhs) ->
    let* lhs' = check_expr ctx lhs in
    let* () = check_lvalue lhs' in
    let* rhs' = check_expr ctx rhs in
    let lhs_type = Option.get lhs'.expr_type in
    let rhs_type = Option.get rhs'.expr_type in
    if is_convertible ~from:rhs_type ~to_:lhs_type then  
      Ok { expr with  
           expr_desc = Assignment (lhs', op, rhs');  
           expr_type = Some lhs_type }  
    else  
      Error (`Type_error "incompatible assignment types")  

  | Conditional (cond, e_true, e_false) ->  
    let* cond' = check_expr ctx cond in  
    let* () = check_scalar cond' in  
    let* t1 = check_expr ctx e_true in  
    let* t2 = check_expr ctx e_false in  
    let rtype = usual_arithmetic_conversions  
        (Option.get t1.expr_type)  
        (Option.get t2.expr_type) in  
    Ok { expr with expr_desc = Conditional (cond', t1, t2);  
                   expr_type = Some rtype }  

  | FunctionCall (fexpr, args) ->  
    let* fexpr' = check_expr ctx fexpr in  
    (match fexpr'.expr_type with  
     | Some (Types.Function (ret_type, params, variadic)) ->  
       let* args' = check_args ctx params variadic args in  
       Ok { expr with expr_desc = FunctionCall (fexpr', args');  
                      expr_type = Some ret_type }  
     | _ -> Error (`Type_error "called object is not a function"))  

  | _ -> (* Further expression forms handled similarly *)  
    Error (`Type_error "expression form not yet supported")
