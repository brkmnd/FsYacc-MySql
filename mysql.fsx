#r "../FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
open Microsoft.FSharp.Text.Lexing
open System

#load "mysql.fs"

module Lexer =
    open System.Collections.Generic
    open System.Text.RegularExpressions
    let lex inStr =
        let tokens = new List<string * Parser.token>()
        let addToken_id (idStr : string) =
            let t2type =
                match idStr.ToLower() with
                | "select" -> Parser.token.KEY_SELECT
                | "as" -> Parser.token.KEY_AS
                | "is" -> Parser.token.KEY_IS
                | "null" -> Parser.token.VAL_NULL
                | "true" -> Parser.token.VAL_TRUE
                | "false" -> Parser.token.VAL_FALSE
                | "in" -> Parser.token.OP_IN
                | "between" -> Parser.token.OP_BETWEEN
                | "sounds" -> Parser.token.OP_SOUNDS
                | "like" -> Parser.token.OP_LIKE
                | "mod" -> Parser.token.OP_MOD_TXT
                | "div" -> Parser.token.OP_DIV_TXT
                | "not" -> Parser.token.OP_NOT_TXT
                | "or" -> Parser.token.OP_OR_TXT
                | "and" -> Parser.token.OP_AND_TXT
                | "all" -> Parser.token.KEY_ALL
                | "any" -> Parser.token.KEY_ANY
                | id -> Parser.token.VAL_ID id
            (idStr,t2type)
        let addToken_delim t =
            let t2type =
                match t with
                | ";" -> Parser.token.DELIM_SCOLON
                | "," -> Parser.token.DELIM_COMMA
                | t ->
                    printfn "token_added(delim): %s" t
                    Parser.token.DELIM_COMMA
            (t,t2type)
        let addToken_op t =
            let t2type =
                match t with
                | "+" -> Parser.token.OP_PLUS
                | "-" -> Parser.token.OP_MINUS
                | "*" -> Parser.token.OP_TIMES
                | "/" -> Parser.token.OP_DIV
                | "%" -> Parser.token.OP_PERC
                | "." -> Parser.token.OP_DOT
                | "||" -> Parser.token.OP_OR
                | "|" -> Parser.token.OP_BOR
                | "<<" -> Parser.token.OP_SHIFT_LEFT
                | ">>" -> Parser.token.OP_SHIFT_RIGHT
                | "!" -> Parser.token.OP_BANG
                | "=" -> Parser.token.OP_EQ
                | "==" -> Parser.token.OP_EQ2
                | "!=" -> Parser.token.OP_NEQ
                | "<>" -> Parser.token.OP_NEQ2
                | "<" -> Parser.token.OP_LT
                | ">" -> Parser.token.OP_GT
                | "<=" -> Parser.token.OP_GEQ
                | ">=" -> Parser.token.OP_LEQ
                | t ->
                    printfn "token_added(op): %s" t
                    Parser.token.OP_DOT
            (t,t2type)
        let addToken xIndex (tGroup : GroupCollection) =
            if tGroup.[1].Value <> "" then
                let v = tGroup.[1].Value
                tokens.Add(v,Parser.token.VAL_HEX v)
            if tGroup.[2].Value <> "" then
                let v = tGroup.[2].Value
                tokens.Add(v,Parser.token.VAL_HEX v)
            if tGroup.[3].Value <> "" then
                tokens.Add(addToken_id tGroup.[3].Value)
            if tGroup.[4].Value <> "" then
                let v = tGroup.[4].Value
                tokens.Add(v,Parser.token.VAL_STRING v)
            if tGroup.[5].Value <> "" then
                let v = tGroup.[5].Value
                tokens.Add(v,Parser.token.VAL_STRING v)
            if tGroup.[6].Value <> "" then
                let v = tGroup.[6].Value
                tokens.Add(v,Parser.token.VAL_NUM v)
            if tGroup.[7].Value <> "" then
                tokens.Add(addToken_delim tGroup.[7].Value)
            if tGroup.[8].Value <> "" then
                tokens.Add(addToken_op tGroup.[8].Value)
        let regToken =
            "x'([^']+)'|"+
            "0x([a-zA-Z0-9]+)|"+
            "([a-zA-Z]+)|"+
            "\"([^\"]*)\"|"+
            "'([^\']*)'|"+
            "([0-9]+)|"+
            "(;|,)|"+
            "(\\|\\|\\||\\+|-|\\*|%|\\.|<<|>>|==|=|<|>|<=|>=|!=|<>|!)|"+
            //ignore
            " +|\\n+"
        let matchF (m : Match) =
            addToken m.Index m.Groups
            ""
        let residueStr = Regex.Replace(inStr,regToken,matchF)
        //check if anything not catched by lexer
        if residueStr <> "" then
            let len = tokens.Count
            if len = 0 then
                failwith "start of input"
            else failwith (fst tokens.[len - 1])
        else
            tokens.Add("eoi",Parser.token.END_OF_INPUT)
            Array.init
                (tokens.Count)
                (fun _ ->
                    let t = tokens.[0]
                    tokens.RemoveAt(0)
                    t
                    )
    let mutable getToken_i = 0
    let getNextToken (tokens : (string * Parser.token) []) i =
        if tokens.Length = 0 || getToken_i >= tokens.Length then
            failwith "trying to get token from empty buffer"
        else
            let t = tokens.[getToken_i]
            getToken_i <- getToken_i + 1
            snd t
    let getPrevTokenVal (tokens : (string * Parser.token) []) =
        if tokens.Length = 0 || getToken_i >= tokens.Length then
            failwith "out of bound token getting"
        elif getToken_i = 0 then
            fst tokens.[0]
        else
            fst tokens.[getToken_i - 1]

let syntax_error_at at =
    sprintf "You have an error in your SQL syntax; check the manual that corresponds to your MariaDB server version for the right syntax to use near '%s' at line 1" at

let parseString (s : string) =
    let lexbuf = LexBuffer<char>.FromString s
    let tokens =
        try Lexer.lex s with
        | Failure error_t ->
            printfn "%s" (syntax_error_at error_t)
            [||]
    Parser.start_entry (Lexer.getNextToken tokens) lexbuf

//parseString "noget"
let prg = parseString "select 'test' as d;"
printfn "%A\n" (prg)
