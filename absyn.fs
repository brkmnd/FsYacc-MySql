module MbSqlAbSyn
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
