module AbSyn
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

let depth2spaces d =
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
    | n -> exec "" n
let rec traverse f = function
    | [] -> ()
    | x::xs -> traverse_qs 0 f x; traverse f xs
and traverse_qs depth f = function
    | Select s ->
        printfn "%sselect" (depth2spaces depth)
        traverse_q_select (depth + 1) f s
    | Options (op_q,op_list) ->
        traverse_qs depth f op_q
    | Union (t,q1,q2) ->
        printfn "%sunion %s" (depth2spaces depth) t
        traverse_qs (depth + 1) f q1
        traverse_qs (depth + 1) f q2
    | _ -> printfn "rest of traverse_qs"
and traverse_q_select depth f = function
    | [SelectNull] -> printfn "%snull" (depth2spaces depth) 
    | [ SelectOptions opt_c
        SelectItems items_c
        SelectInto into_c
        SelectFrom from_c
        SelectWhere where_c
        SelectGroup group_c
        SelectHaving having_c
        SelectWindow window_c ] ->
            printfn "%sitems:" (depth2spaces depth)
            traverse_exp_list (depth + 1) f items_c
            printfn "%sfrom:" (depth2spaces depth)
            traverse_exp_list (depth + 1) f from_c
            printfn "%swhere:" (depth2spaces depth)
            traverse_exp (depth + 1) f where_c
    | _ -> printfn "rest of traverse_q_select"
and traverse_exp_list depth f = function
    | [] -> ()
    | expr::exprs ->
        traverse_exp depth f  expr
        traverse_exp_list depth f  exprs
and traverse_exp depth f = function
    | Expr.Null -> printfn "%snull" (depth2spaces depth)
    | Node v -> printfn "%snode(%s)" (depth2spaces depth) v
    | NodeTyped (t,v) -> printfn "%snode<%s>(%s)" (depth2spaces depth) t v
    | Binary (op,l,r) ->
        printfn "%s%s:" (depth2spaces depth) op
        traverse_exp (depth + 1) f l
        traverse_exp (depth + 1) f r
    | ExprList elist ->
        printfn "%slist:" (depth2spaces depth)
        traverse_exp_list (depth + 1) f elist
    | ExprListTyped (t,elist) ->
        printfn "%slist<%s>:" (depth2spaces depth) t
        traverse_exp_list (depth + 1) f elist
    | SubQ q ->
        printfn "%ssubq:" (depth2spaces depth)
        traverse_qs (depth + 1) f q
    | FunctionCall (id,args) ->
        printfn "%s%s" (depth2spaces depth) "(call)"
        traverse_exp (depth + 1) f id
        printfn "%s%s" (depth2spaces depth) "(args)"
        traverse_exp (depth + 1) f args
    | expr -> printfn "%s%A" (depth2spaces depth) expr
