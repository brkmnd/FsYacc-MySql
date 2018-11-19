module MbSqlTraverse
open MbSqlAbSyn
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

(* Temporary test generators
 * *)    
let go_string (l) =
    let d2s d =
        let s = "  "
        let rec exec acc = function
            | n when n <= 0 -> acc
            | n -> exec (acc + s) (n - 1)
        match d with
        | 0  -> ""
        | 1  -> s
        | 2  -> s+s
        | 3  -> s+s+s
        | 4  -> s+s+s+s
        | 5  -> s+s+s+s+s
        | 6  -> s+s+s+s+s+s
        | 7  -> s+s+s+s+s+s+s
        | 8  -> s+s+s+s+s+s+s+s
        | 9  -> s+s+s+s+s+s+s+s+s
        | 10 -> s+s+s+s+s+s+s+s+s+s
        | 11 -> s+s+s+s+s+s+s+s+s+s+s
        | 12 -> s+s+s+s+s+s+s+s+s+s+s+s
        | 13 -> s+s+s+s+s+s+s+s+s+s+s+s+s
        | 14 -> s+s+s+s+s+s+s+s+s+s+s+s+s+s
        | 15 -> s+s+s+s+s+s+s+s+s+s+s+s+s+s+s
        | 16 -> s+s+s+s+s+s+s+s+s+s+s+s+s+s+s+s
        | n -> exec "" n
    let travF depth acc (x : TreeVal<string>) =
        let d0 = d2s depth
        if x.vname = "options" then
            Array.fold (fun acc x -> acc + x) "" x.vargs
        elif x.vtype = "qoption" then
            let s1 = sprintf "%s%s<option>\n" d0 x.vname
            let s2 = x.vargs.[0]
            acc + s1 + s2
        elif x.vname = "union" then
            let s0 = sprintf "%sunion<%s>\n" d0 x.vtype
            let s1 = sprintf "%s%s" (d2s (depth + 1)) x.vargs.[0]
            let s2 = sprintf "%s%s" (d2s (depth + 1)) x.vargs.[1]
            s0 + s1 + s2
        elif x.vname = "select" then
            let s1 = sprintf "%s<%s>\n" x.vname x.vtype
            let s2 = sprintf "%sselect.options:\n%s" d0 x.vargs.[0]
            let s3 = sprintf "%sselect.items:\n%s" d0 x.vargs.[1]
            let s4 = sprintf "%sselect.into:\n%s" d0 x.vargs.[2]
            let s5 = sprintf "%sselect.from:\n%s" d0 x.vargs.[3]
            let s6 = sprintf "%sselect.where:\n%s" d0 x.vargs.[4]
            let s7 = sprintf "%sselect.group:\n%s" d0 x.vargs.[5]
            let s8 = sprintf "%sselect.having:\n%s" d0 x.vargs.[6]
            let s9 = sprintf "%sselect.window:\n%s" d0 x.vargs.[7]
            s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9
        elif x.vtype = "unary" then
            let s1 = sprintf "%s%s<unary>\n" (d2s depth) x.vname
            let s2 = x.vargs.[0]
            s1 + s2
        elif x.vtype = "binary" then
            let s1 = sprintf "%s%s<binary>\n" (d2s depth) x.vname
            let s2 = x.vargs.[0]
            let s3 = x.vargs.[1]
            s1 + s2 + s3
        elif x.vtype = "call" then
            let s1 = sprintf "%s%s<call>\n" (d2s depth) x.vname
            let s_id = x.vargs.[0]
            let s_args = x.vargs.[1]
            s1 + s_id + s_args
        elif x.vname = "list" then
            let s1 = sprintf "%slist<%s>\n" (d2s depth) x.vtype
            let s2 = Array.fold (fun acc x -> acc + x) s1 x.vargs
            s2
        elif x.vname = "node" && x.vtype <> "" then
            sprintf "%snode<%s>%s\n" d0 x.vtype x.vval
        elif x.vname = "node" then
            sprintf "%snode<>%s\n" d0 x.vval
        else
            acc
    Go<string>.gen (travF,"",l)
