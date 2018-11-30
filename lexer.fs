module MbSqlLexer
open System.Collections.Generic
open System.Text.RegularExpressions
let mutable getToken_i = 0
let removeMultComment =
    let (|Sub|Sub2|Empty|) str =
        if str = "" then Empty
        elif str.Length = 1 then Sub (string str.[0],str.Substring(1))
        else Sub2 (string str.[0],str.[0 .. 1],str.Substring(1))
    let rec exec acc commOn = function
        //return acc, comment_open
        | Empty -> (acc,commOn)
        | Sub (c,rest) -> exec (acc + c) commOn rest
        | Sub2 (c,s2,rest) when commOn ->
            if s2 = "*/" then exec acc false (rest.Substring(1))
            else exec acc true rest
        | Sub2 (c,s2,rest) ->
            if s2 = "/*" then exec acc true (rest.Substring(1))
            else exec (acc + c) false rest
    exec "" false
let lex inStr =
    let tokens = new List<string * MbSqlParser.token>()
    let addToken_id (idStr : string) =
        let t2type =
            match idStr.ToLower() with
            | "all" -> MbSqlParser.token.KEY_ALL
            | "and" -> MbSqlParser.token.OP_AND_TXT
            | "any" -> MbSqlParser.token.KEY_ANY
            | "as" -> MbSqlParser.token.KEY_AS
            | "asc" -> MbSqlParser.token.KEY_ASC
            | "between" -> MbSqlParser.token.OP_BETWEEN
            | "by" -> MbSqlParser.token.KEY_BY
            | "cross" -> MbSqlParser.token.OP_CROSS
            | "columns" -> MbSqlParser.token.KEY_COLUMNS
            | "char" -> MbSqlParser.token.KEY_CHAR
            | "cast" -> MbSqlParser.token.KEY_CAST
            | "convert" -> MbSqlParser.token.KEY_CONVERT
            | "select" -> MbSqlParser.token.KEY_SELECT
            | "from" -> MbSqlParser.token.KEY_FROM
            | "where" -> MbSqlParser.token.KEY_WHERE
            | "is" -> MbSqlParser.token.KEY_IS
            | "null" -> MbSqlParser.token.VAL_NULL
            | "true" -> MbSqlParser.token.VAL_TRUE
            | "false" -> MbSqlParser.token.VAL_FALSE
            | "dual" -> MbSqlParser.token.VAL_DUAL
            | "in" -> MbSqlParser.token.OP_IN
            | "into" -> MbSqlParser.token.OP_INTO
            | "sounds" -> MbSqlParser.token.OP_SOUNDS
            | "like" -> MbSqlParser.token.OP_LIKE
            | "mod" -> MbSqlParser.token.OP_MOD_TXT
            | "div" -> MbSqlParser.token.OP_DIV_TXT
            | "not" -> MbSqlParser.token.OP_NOT_TXT
            | "or" -> MbSqlParser.token.OP_OR_TXT
            | "join" -> MbSqlParser.token.OP_JOIN
            | "inner" -> MbSqlParser.token.OP_INNER
            | "straight_join" -> MbSqlParser.token.OP_STRAIGHT_JOIN
            | "natural" -> MbSqlParser.token.OP_NATURAL
            | "left" -> MbSqlParser.token.OP_LEFT
            | "right" -> MbSqlParser.token.OP_RIGHT
            | "on" -> MbSqlParser.token.OP_ON
            | "group" -> MbSqlParser.token.OP_GROUP
            | "order" -> MbSqlParser.token.KEY_ORDER
            | "limit" -> MbSqlParser.token.KEY_LIMIT
            | "desc" -> MbSqlParser.token.KEY_DESC
            | "using" -> MbSqlParser.token.OP_USING
            | "outfile" -> MbSqlParser.token.KEY_OUTFILE
            | "force" -> MbSqlParser.token.KEY_FORCE
            | "ignore" -> MbSqlParser.token.KEY_IGNORE
            | "key" -> MbSqlParser.token.KEY_KEY
            | "keys" -> MbSqlParser.token.KEY_KEYS
            | "unique" -> MbSqlParser.token.KEY_UNIQUE
            | "json_table" -> MbSqlParser.token.OP_JSON_TABLE
            | "for" -> MbSqlParser.token.KEY_FOR
            | "ordinality" -> MbSqlParser.token.NOKEY_ORDINALITY
            | "nested" -> MbSqlParser.token.NOKEY_NESTED
            | "path" -> MbSqlParser.token.NOKEY_PATH
            | "union" -> MbSqlParser.token.OP_UNION
            | "offset" -> MbSqlParser.token.KEY_OFFSET
            | id -> MbSqlParser.token.VAL_ID id
        (idStr,t2type)
    let addToken_delim t =
        let t2type =
            match t with
            | ";" -> MbSqlParser.token.DELIM_SCOLON
            | "," -> MbSqlParser.token.DELIM_COMMA
            | t ->
                MbSqlParser.token.DELIM_COMMA
        (t,t2type)
    let addToken_op t =
        let t2type =
            match t with
            | "+" -> MbSqlParser.token.OP_PLUS
            | "-" -> MbSqlParser.token.OP_MINUS
            | "*" -> MbSqlParser.token.OP_TIMES
            | "/" -> MbSqlParser.token.OP_DIV
            | "%" -> MbSqlParser.token.OP_PERC
            | "." -> MbSqlParser.token.OP_DOT
            | "||" -> MbSqlParser.token.OP_OR
            | "&&" -> MbSqlParser.token.OP_AND
            | "|" -> MbSqlParser.token.OP_BOR
            | "<<" -> MbSqlParser.token.OP_SHIFT_LEFT
            | ">>" -> MbSqlParser.token.OP_SHIFT_RIGHT
            | "!" -> MbSqlParser.token.OP_BANG
            | "=" -> MbSqlParser.token.OP_EQ
            | "==" -> MbSqlParser.token.OP_EQ2
            | "!=" -> MbSqlParser.token.OP_NEQ
            | "<>" -> MbSqlParser.token.OP_NEQ2
            | "<" -> MbSqlParser.token.OP_LT
            | ">" -> MbSqlParser.token.OP_GT
            | "<=" -> MbSqlParser.token.OP_GEQ
            | ">=" -> MbSqlParser.token.OP_LEQ
            | "(" -> MbSqlParser.token.PAR_LPAR
            | ")" -> MbSqlParser.token.PAR_RPAR
            | "{" -> MbSqlParser.token.PAR_LBRACE
            | "}" -> MbSqlParser.token.PAR_RBRACE
            | t -> MbSqlParser.token.OP_DOT
        (t,t2type)
    let addToken xIndex (tGroup : GroupCollection) =
        let i = 1
        if tGroup.[i].Value <> "" then
            let v = tGroup.[i].Value
            tokens.Add(v,MbSqlParser.token.VAL_HEX v)
        let i = i + 1
        if tGroup.[i].Value <> "" then
            let v = tGroup.[i].Value
            tokens.Add(v,MbSqlParser.token.VAL_HEX v)
        let i = i + 1
        if tGroup.[i].Value <> "" then
            tokens.Add(addToken_id tGroup.[i].Value)
        let i = i + 1
        if tGroup.[i].Value <> "" then
            let v = tGroup.[i].Value
            tokens.Add(v,MbSqlParser.token.VAL_STRING v)
        let i = i + 1
        if tGroup.[i].Value <> "" then
            let v = tGroup.[i].Value
            tokens.Add(v,MbSqlParser.token.VAL_STRING v)
        let i = i + 1
        if tGroup.[i].Value <> "" then
            let v = tGroup.[i].Value
            tokens.Add(v,MbSqlParser.token.VAL_FLOAT v)
        let i = i + 1
        if tGroup.[i].Value <> "" then
            let v = tGroup.[i].Value
            tokens.Add(v,MbSqlParser.token.VAL_NUM v)
        let i = i + 1
        if tGroup.[i].Value <> "" then
            tokens.Add(addToken_delim tGroup.[i].Value)
        let i = i + 1
        if tGroup.[i].Value <> "" then
            tokens.Add(addToken_op tGroup.[i].Value)
    let regToken =
        //comments
        "-- [^\\n]*|\\#[^\\n]*|\\/\\*[^\\*]*\\*\\/|"+
        "x'([^']+)'|"+
        "0x([a-zA-Z0-9]+)|"+
        "([a-zA-Z_][a-zA-Z0-9_]*)|"+
        "\"([^\"]*)\"|"+
        "'([^\']*)'|"+
        "([0-9]*\\.[0-9]+)|"+
        "([0-9]+)|"+
        "(;|,)|"+
        "(\\|\\||\\&\\&|\\+|-|\\*|\\/|%|\\.|<<|>>|==|=|<|>|<=|>=|!=|<>|!|\\(|\\)|\\{|\\}|\\[|\\])|"+
        //ignore
        " +|\\n+"
    let matchF (m : Match) =
        addToken m.Index m.Groups
        ""
    let residueStr =
        let (removedComm,openComm) = removeMultComment inStr
        if openComm then
            failwith "end of input"
        getToken_i <- 0
        Regex.Replace(removedComm,regToken,matchF)
    //check if anything not catched by lexer
    if residueStr <> "" then
        let len = tokens.Count
        if len = 0 then
            failwith "start of input" //start of input
        else failwith (fst tokens.[len - 1])
    else
        tokens.Add("eoi",MbSqlParser.token.END_OF_INPUT)
        Array.init
            (tokens.Count)
            (fun _ ->
                let t = tokens.[0]
                tokens.RemoveAt(0)
                t
                )
let getNextToken (tokens : (string * MbSqlParser.token) []) i =
    if tokens.Length = 0 || getToken_i >= tokens.Length then
        failwith "trying to get token from empty buffer"
    else
        let t = tokens.[getToken_i]
        getToken_i <- getToken_i + 1
        snd t
let getPrevTokenVal (tokens : (string * MbSqlParser.token) []) =
    if tokens.Length = 0 || getToken_i > tokens.Length then
        failwith "out of bound token getting"
    elif getToken_i = 0 then
        fst tokens.[0]
    else
        fst tokens.[getToken_i - 1]
