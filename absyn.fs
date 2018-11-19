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
        | SelectFrom of Expr
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
    type TreeVal<'T> = {vname:string;vtype:string;vval:string;vargs:'T []}
    let rec traverse f acc vfs = function
        | [] -> acc
        | x::xs ->
            let acc1 = traverse_qs 0 f acc vfs x
            let acc2 = traverse f acc1 vfs xs
            acc2
    and traverse_qs depth f acc vfs q =
        let (v_n,v_nt,v_nta,v_ntv,v_ntva) = vfs
        match q with
        | Select s ->
            traverse_q_select (depth + 1) f acc vfs s
        | Options (op_q,op_list) ->
            //take care of op-list (order and so on)
            let acc0 = traverse_qs depth f acc vfs op_q
            let args =
                List.fold
                    (fun acc_args x ->
                        match x with
                        | OptOrder expr ->
                            let arg = v_nta "order" "qoption" [|traverse_exp (depth+1) f acc vfs expr|]
                            acc_args @ [f (depth) acc arg]
                        | OptLimit expr ->
                            let arg = v_nta "limit" "qoption" [|traverse_exp (depth+1) f acc vfs expr|]
                            acc_args @ [f (depth) acc arg]
                        | OptLocking expr ->
                            let arg = v_nta "locking" "qoption" [|traverse_exp (depth+1) f acc vfs expr|]
                            acc_args @ [f (depth) acc arg]
                        )
                    [acc0]
                    op_list
            let acc2 = f depth acc (v_nta "options" "query" (List.toArray args))
            acc2
        | Union (t,q1,q2) ->
            let acc0 = traverse_qs (depth + 1) f acc vfs q1
            let acc1 = traverse_qs (depth + 1) f acc vfs q2
            let acc2 = f depth acc (v_nta "union" "query" [|acc0;acc1|])
            acc2
        | _ ->
            printfn "not-imp-yet traverse_qs"
            acc
    and traverse_q_select depth f acc vfs = function
        | [SelectNull] ->
            //printfn "%snull" (depth2spaces depth)
            let (v_n,_,_,_,_) = vfs
            f depth acc (v_n "null")
        | [ SelectOptions opt_c
            SelectItems items_c
            SelectInto into_c
            SelectFrom from_c
            SelectWhere where_c
            SelectGroup group_c
            SelectHaving having_c
            SelectWindow window_c ] ->
                let (v_n,v_nt,v_nta,v_ntv,v_ntva) = vfs
                let acc0 = traverse_exp (depth + 1) f acc vfs (ExprList opt_c)
                let acc1 = traverse_exp (depth + 1) f acc vfs (ExprList items_c)
                let acc2 = traverse_exp (depth + 1) f acc vfs into_c
                let acc3 = traverse_exp (depth + 1) f acc vfs from_c
                let acc4 = traverse_exp (depth + 1) f acc vfs where_c
                let acc5 = traverse_exp (depth + 1) f acc vfs group_c
                let acc6 = traverse_exp (depth + 1) f acc vfs having_c
                let acc7 = traverse_exp (depth + 1) f acc vfs window_c
                let acc8 = f depth acc (v_nta "select" "query" [|acc0;acc1;acc2;acc3;acc4;acc5;acc6;acc7|])
                acc8
        | _ ->
            printfn "not imp yet - traverse_q_select"
            acc
    and traverse_exp_list depth f acc vfs l =
        List.map (traverse_exp depth f acc vfs) l
    and traverse_exp depth f acc vfs expr =
        let (v_n,v_nt,v_nta,v_ntv,v_ntva) = vfs
        match expr with
        | Expr.Null ->
            f depth acc (v_ntv "node" "" "null")
        | Node v ->
            f depth acc (v_ntv "node" "" v)
        | NodeTyped (t,v) ->
            f depth acc (v_ntv "node" t v)
        | Unary (op,operand) ->
            let acco = traverse_exp (depth+1) f acc vfs operand
            let acc0 = f depth acc (v_nta op "unary" [|acco|])
            acc0
        | Binary (op,l,r) ->
            let accl = traverse_exp (depth+1) f acc vfs l
            let accr = traverse_exp (depth+1) f acc vfs r
            let acc0 = f depth acc (v_nta op "binary" [|accl;accr|])
            acc0
        | ExprList elist ->
            let acc_list = traverse_exp_list (depth + 1) f acc vfs elist
            let acc1 = f depth acc (v_nta "list" "" (List.toArray acc_list))
            acc1
        | ExprListTyped (t,elist) ->
            let acc_list = traverse_exp_list (depth + 1) f acc vfs elist
            let acc1 = f depth acc (v_nta "list" t (List.toArray acc_list))
            acc1
        | SubQ q ->
            let acc0 = traverse_qs (depth + 1) f acc vfs q
            let acc1 = f depth acc (v_nta "subq" "unary" [|acc0|])
            acc1
        | FunctionCall (id,args) ->
            let acc_id = traverse_exp (depth + 1) f acc vfs id
            let acc_args = traverse_exp (depth + 1) f acc vfs args
            let acc0 = f depth acc (v_nta "fun" "call" [|acc_id;acc_args|])
            acc0

        | Temp ->
            f depth acc (v_nt "node" "temp")
        | expr ->
            printfn "hertil:%A" expr
            acc
    type Go<'T> =
        static member gen (f,acc,l) =
            let val_name n = {vname=n;vtype="";vval="";vargs=[||]} : TreeVal<'T>
            let val_name_type n t = {vname=n;vtype=t;vval="";vargs=[||]} : TreeVal<'T>
            let val_name_type_args n t args = {vname=n;vtype=t;vval="";vargs=args} : TreeVal<'T>
            let val_name_type_v n t v = {
                vname = n
                vtype = t
                vval = v
                vargs = [||]
                }
            let val_name_type_v_args n t v args = {
                vname = n
                vtype = t
                vval = v
                vargs = args
                }
            let fs = (val_name,val_name_type,val_name_type_args,val_name_type_v,val_name_type_v_args)
            traverse f acc fs l
