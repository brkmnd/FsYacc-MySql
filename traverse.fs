module MbSqlTraverse
open MbSqlAbSyn
type TreeVal<'T> = {vname:string;vtype:string;vval:string;vargs:'T []}
let add2acc f acc v =
    match acc with
    | Some acc0 -> Some (f acc0 v)
    | None -> None
let args2arr args =
    let args0 = Array.foldBack (fun x acc -> match x with Some a -> a::acc | None -> acc) args []
    List.toArray args0
let largs2arr largs =
    let args0 = List.foldBack (fun x acc -> match x with Some a -> a::acc | None -> acc) largs []
    List.toArray args0
let rec traverse f vfs acc = function
    | [] -> acc
    | x::xs ->
        let acc1 = traverse_qs 0 f vfs acc x
        let acc2 = traverse f vfs acc1 xs
        acc2
and traverse_qs depth f vfs acc q =
    let (v_n,v_nt,v_nta,v_ntv,v_ntva) = vfs
    match q with
    | Select s ->
        traverse_q_select (depth + 1) f vfs acc s
    | Options (op_q,op_list) ->
        //take care of op-list (order and so on)
        let acc0 = traverse_qs depth f vfs acc op_q
        let args =
            List.fold
                (fun acc_args x ->
                    match x with
                    | OptOrder expr ->
                        let arg =  args2arr [|traverse_exp (depth+1) f vfs acc expr|]
                        acc_args @ [add2acc (f depth) acc (v_nta "order" "qoption" arg)]
                    | OptLimit expr ->
                        let arg = args2arr [|traverse_exp (depth+1) f vfs acc expr|]
                        acc_args @ [add2acc (f depth) acc (v_nta "limit" "qoption" arg)]
                    | OptLocking expr ->
                        let arg = args2arr [|traverse_exp (depth+1) f vfs acc expr|]
                        acc_args @ [add2acc (f depth) acc (v_nta "locking" "qoption" arg)]
                    )
                [acc0]
                op_list
        let args0 = largs2arr args
        let acc2 = add2acc (f depth) acc (v_nta "options" "query" args0)
        acc2
    | Union (t,q1,q2) ->
        let acc0 = traverse_qs (depth + 1) f vfs acc q1
        let acc1 = traverse_qs (depth + 1) f vfs acc q2
        let args = args2arr [|acc0;acc1|]
        let acc2 = add2acc (f depth) acc (v_nta "union" "query" args)
        acc2
    | _ -> acc
and traverse_q_select depth f vfs acc = function
    | [SelectNull] ->
        let (v_n,_,_,_,_) = vfs
        add2acc (f depth) acc (v_n "null")
    | [ SelectOptions opt_c
        SelectItems items_c
        SelectInto into_c
        SelectFrom from_c
        SelectWhere where_c
        SelectGroup group_c
        SelectHaving having_c
        SelectWindow window_c ] ->
            let (v_n,v_nt,v_nta,v_ntv,v_ntva) = vfs
            let acc0 = traverse_exp (depth + 1) f vfs acc (ExprList opt_c)
            let acc1 = traverse_exp (depth + 1) f vfs acc (ExprList items_c)
            let acc2 = traverse_exp (depth + 1) f vfs acc into_c
            let acc3 = traverse_exp (depth + 1) f vfs acc from_c
            let acc4 = traverse_exp (depth + 1) f vfs acc where_c
            let acc5 = traverse_exp (depth + 1) f vfs acc group_c
            let acc6 = traverse_exp (depth + 1) f vfs acc having_c
            let acc7 = traverse_exp (depth + 1) f vfs acc window_c
            let args =
                match (acc0,acc1,acc2,acc3,acc4,acc5,acc6,acc7) with
                | (Some a0,Some a1,Some a2,Some a3,Some a4,Some a5,Some a6,Some a7) ->
                    [|a0;a1;a2;a3;a4;a5;a6;a7|]
                | _ -> [||]
            let acc8 = add2acc (f depth) acc (v_nta "select" "query" args)
            acc8
    | _ -> None
and traverse_exp_list depth f vfs acc l =
    let l0 = List.map (traverse_exp depth f vfs acc) l
    let l1 = List.foldBack (fun x acc0 -> match x with Some x0 -> x0::acc0 | None -> acc0) l0 []
    List.toArray l1
and traverse_exp depth f vfs acc expr =
    let (v_n,v_nt,v_nta,v_ntv,v_ntva) = vfs
    match expr with
    | Expr.Null ->
        add2acc (f depth) acc (v_ntv "node" "" "null")
    | Node v ->
        add2acc (f depth) acc (v_ntv "node" "" v)
    | NodeTyped (t,v) ->
        add2acc (f depth) acc (v_ntv "node" t v)
    | Unary (op,operand) ->
        let arg =
            match traverse_exp (depth+1) f vfs acc operand with
            | Some a -> [|a|]
            | _ -> [||]
        let acc1 = add2acc (f depth) acc (v_nta op "unary" arg)
        acc1
    | Binary (op,l,r) ->
        let accl = traverse_exp (depth+1) f vfs acc l
        let accr = traverse_exp (depth+1) f vfs acc r
        let args =
            match (accl,accr) with
            | (Some l,Some r) -> [|l;r|]
            | _ -> [||]
        let acc0 = add2acc (f depth) acc (v_nta op "binary" args)
        acc0
    | ExprList elist ->
        let args = traverse_exp_list (depth + 1) f vfs acc elist
        let acc1 = add2acc (f depth) acc (v_nta "list" "" args)
        acc1
    | ExprListTyped (t,elist) ->
        let args = traverse_exp_list (depth + 1) f vfs acc elist
        let acc1 = add2acc (f depth) acc (v_nta "list" t args)
        acc1
    | SubQ q ->
        let acc0 = traverse_qs (depth + 1) f vfs acc q
        let acc1 = add2acc (f depth) acc (v_nta "subq" "unary" (args2arr [|acc0|]))
        acc1
    | FunctionCall (id,args) ->
        let acc_id = traverse_exp (depth + 1) f vfs acc id
        let acc_args = traverse_exp (depth + 1) f vfs acc args
        let acc0 = add2acc (f depth) acc (v_nta "fun" "call" (args2arr [|acc_id;acc_args|]))
        acc0
    | Empty -> None

    | Temp -> add2acc (f depth) acc (v_nt "node" "temp")
    | expr -> None
