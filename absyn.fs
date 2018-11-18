namespace MbSql
module AbSyn =
    type Expr =
        | Binary of string * Expr * Expr
        | Unary of string * Expr
        | Node of string
        | NodeTyped of string * string
        | ExprList of Expr list
        | ExprListTyped of string * (Expr list)
        | FunctionCall of Expr * Expr
        | FunctionCreate of Expr * Expr
        | SubQ of Qs
        | Null
        | Temp
    and Q_Select =
        | SelectNull
        | SelectOptions of Expr list
        | SelectItems of Expr list
        | SelectInto of Expr
        | SelectFrom of Expr list
        | SelectWhere of Expr
        | SelectGroup of Expr
        | SelectHaving of Expr
        | SelectWindow of Expr
    and Qs_Option =
        | OptOrder of Expr
        | OptLimit of Expr
        | OptLocking of Expr
    and Qs =
        | Select of Q_Select list
        | Options of Qs * (Qs_Option list)
        | Union of string * Qs * Qs
        | Error of string
        | Null
module Traverse =
    open AbSyn
    type TreeVal<'T> = {vname:string;vtype:string;args:'T list}
    //type TreeFun<'T> = int -> 'T -> TreeVal -> 'T
    let rec traverse f acc (v_n,v_nt,v_nta) = function
        | [] -> acc
        | x::xs ->
            let acc1 = traverse_qs 0 f acc (v_n,v_nt,v_nta) x
            let acc2 = traverse f acc1 (v_n,v_nt,v_nta) xs
            acc2
    and traverse_qs depth f acc (v_n,v_nt,v_nta) = function
        | Select s ->
            //printfn "%sselect" (depth2spaces depth)
            let acc0 = f depth acc (v_n "select")
            traverse_q_select (depth + 1) f acc0 (v_n,v_nt,v_nta) s
        | Options (op_q,op_list) ->
            //take care of op-list (order and so on)
            let fs = (v_n,v_nt,v_nta)
            let acc0 = f depth acc (v_n "options")
            let acc1 = traverse_qs depth f acc0 (v_n,v_nt,v_nta) op_q
            let acc2 =
                List.fold
                    (fun acc x ->
                        match x with
                        | OptOrder expr ->
                            let v = v_n "order"
                            f depth (traverse_exp (depth+1) f acc fs expr) v
                        | OptLimit expr ->
                            let v = v_n "limit"
                            f depth (traverse_exp (depth+1) f acc fs expr) v
                        | OptLocking expr ->
                            let v = v_n "locking"
                            f depth (traverse_exp (depth+1) f acc fs expr) v
                        )
                    acc1
                    op_list
            acc2
        | Union (t,q1,q2) ->
            let acc0 = f depth acc (v_n "union")
            let acc1 = traverse_qs (depth + 1) f acc0 (v_n,v_nt,v_nta) q1
            let acc2 = traverse_qs (depth + 1) f acc1 (v_n,v_nt,v_nta) q2
            acc
        | _ ->
            printfn "not-imp-yet traverse_qs"
            acc
    and traverse_q_select depth f acc (v_n,v_nt,v_nta) = function
        | [SelectNull] ->
            //printfn "%snull" (depth2spaces depth)
            f depth acc (v_n "null")
        | [ SelectOptions opt_c
            SelectItems items_c
            SelectInto into_c
            SelectFrom from_c
            SelectWhere where_c
            SelectGroup group_c
            SelectHaving having_c
            SelectWindow window_c ] ->
                //printfn "%sitems:" (depth2spaces depth)
                let acc0 = f depth acc (v_n "items")
                let acc1 = traverse_exp_list (depth + 1) f acc0 (v_n,v_nt,v_nta) items_c
                //printfn "%sfrom:" (depth2spaces depth)
                let acc2 = f depth acc1 (v_n "from")
                let acc3 = traverse_exp_list (depth + 1) f acc2 (v_n,v_nt,v_nta) from_c
                //printfn "%swhere:" (depth2spaces depth)
                let acc4 = f depth acc3 (v_n "where")
                let acc5 = traverse_exp (depth + 1) f acc4 (v_n,v_nt,v_nta) where_c
                acc5
        | _ ->
            printfn "not imp yet - traverse_q_select"
            acc
    and traverse_exp_list depth f acc (v_n,v_nt,v_nta) = function
        | [] -> acc
        | expr::exprs ->
            let acc0 = traverse_exp depth f acc (v_n,v_nt,v_nta) expr
            let acc1 = traverse_exp_list depth f acc0 (v_n,v_nt,v_nta) exprs
            acc1
    and traverse_exp depth f acc (v_n,v_nt,v_nta) = function
        | Expr.Null -> f depth acc (v_n "null")
            //printfn "%snull" (depth2spaces depth)
        | Node v -> f depth acc (v_n "node")
        | NodeTyped (t,v) -> f depth acc (v_n "node")
        | Binary (op,l,r) ->
            //printfn "%s%s:" (depth2spaces depth) op
            let accl = traverse_exp depth f acc (v_n,v_nt,v_nta) l
            let accr = traverse_exp depth f accl (v_n,v_nt,v_nta) r
            let acc0 = f depth acc (v_nta op "binary" [acc])
            //let acc1 = traverse_exp (depth + 1) f acc0 l
            //let acc2 = traverse_exp (depth + 1) f acc1 r
            acc
        | ExprList elist ->
            //printfn "%slist:" (depth2spaces depth)
            //let acc0 = f depth acc (val_name "list")
            //let acc1 = traverse_exp_list (depth + 1) f acc0 elist
            acc
        | ExprListTyped (t,elist) ->
            //printfn "%slist<%s>:" (depth2spaces depth) t
            //let acc0 = f depth acc (val_name_type "list" t)
            //let acc1 = traverse_exp_list (depth + 1) f acc0 elist
            acc
        | SubQ q ->
            //printfn "%ssubq:" (depth2spaces depth)
            //let acc0 = f depth acc (val_name "sub-q")
            //let acc1 = traverse_qs (depth + 1) f acc0 q
            acc
        | FunctionCall (id,args) ->
            //printfn "%s%s" (depth2spaces depth) "(call)"
            //let acc_id = traverse_exp (depth + 1) f acc id
            //let acc_args = traverse_exp (depth + 1) f acc args
            //let acc0 = f depth acc (val_name_type_v "call" "" acc_id acc_args)
            //printfn "%s%s" (depth2spaces depth) "(args)"
            //traverse_exp (depth + 1) f args
            acc
        | expr ->
            acc
    type Go<'T> =
        static member gen (f,acc,l) =
            let val_name n = {vname=n;vtype="";args=[]} : TreeVal<'T>
            let val_name_type n t = {vname=n;vtype=t;args=[]} : TreeVal<'T>
            let val_name_type_args n t args = {vname=n;vtype=t;args=args} : TreeVal<'T>
            let fs = (val_name,val_name_type,val_name_type_args)
            traverse f acc fs l
