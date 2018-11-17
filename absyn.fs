module AbSyn
type Expr =
    | Binary of string * Expr * Expr
    | Unary of string * Expr
    | Node of string
    | NodeTyped of string * string
    | ExprList of Expr list
    | ExprListTyped of string * (Expr list)
    | Function of Expr * Expr
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

let add2path p = p + "  "
let rec traverse f = function
    | [] -> ()
    | x::xs ->
        let path = ""
        traverse_qs path f x; traverse f xs
and traverse_qs path f = function
    | Select s ->
        printfn "%sselect" path
        let path = add2path path
        traverse_q_select path f s
    | Options (op_q,op_list) ->
        traverse_qs path f op_q
    | _ -> printfn "hertil-qs"
and traverse_q_select path f = function
    | [SelectNull] -> printfn "hertil-select"
    | [ SelectOptions opt_c
        SelectItems items_c
        SelectInto into_c
        SelectFrom from_c
        SelectWhere where_c
        SelectGroup group_c
        SelectHaving having_c
        SelectWindow window_c ] ->
            printfn "%sitems: %A" path (traverse_exp_list path f items_c)
    | _ -> printfn "heritl"
and traverse_exp_list path f = function
    | i -> printf "%A" i
and traverse_exp path f = function
    | _ -> printfn "test"
