#r "../FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
open Microsoft.FSharp.Text.Lexing
open System

#load "mysql.fs"

module Lexer =
    open System.Collections.Generic
    open System.Text.RegularExpressions
    let lex inStr =
        let tokens = new List<Parser.token>()
        let addToken_id (idStr : string) =
            match idStr.ToLower() with
            | "select" -> Parser.token.KEY_SELECT
            | "as" -> Parser.token.KEY_AS
            | "is" -> Parser.token.KEY_IS
            | "null" -> Parser.token.VAL_NULL
            | "true" -> Parser.token.VAL_TRUE
            | "false" -> Parser.token.VAL_FALSE
            | id -> Parser.token.VAL_ID id
        let addToken_delim = function
            | ";" -> Parser.token.DELIM_SCOLON
            | "," -> Parser.token.DELIM_COMMA
            | t ->
                printfn "token_added(delim): %s" t
                Parser.token.DELIM_COMMA
        let addToken_op = function
            | "+" -> Parser.token.OP_PLUS
            | "-" -> Parser.token.OP_MINUS
            | "*" -> Parser.token.OP_TIMES
            | "/" -> Parser.token.OP_DIV
            | "%" -> Parser.token.OP_PERC
            | "." -> Parser.token.OP_DOT
            | t ->
                printfn "token_added(op): %s" t
                Parser.token.OP_DOT
        let addToken xIndex (tGroup : GroupCollection) =
            if tGroup.[1].Value <> "" then
                tokens.Add(addToken_id tGroup.[1].Value)
            if tGroup.[2].Value <> "" then
                tokens.Add(Parser.token.VAL_STRING tGroup.[2].Value)
            if tGroup.[3].Value <> "" then
                tokens.Add(Parser.token.VAL_STRING tGroup.[3].Value)
            if tGroup.[4].Value <> "" then
                tokens.Add(Parser.token.VAL_NUM tGroup.[4].Value)
            if tGroup.[5].Value <> "" then
                tokens.Add(addToken_delim tGroup.[5].Value)
            if tGroup.[6].Value <> "" then
                tokens.Add(addToken_op tGroup.[6].Value)
        let regToken =
            "([a-zA-Z]+)|"+
            "\"([^\"]*)\"|"+
            "'([^\']*)'|"+
            "([0-9]+)|"+
            "(;|,)|"+
            "(\\+|-|\\*|%|\\.)|"+
            //ignore
            " +|\\n+"
        let matchF (m : Match) =
            addToken m.Index m.Groups
            ""
        let residueStr = Regex.Replace(inStr,regToken,matchF)
        //check if anything not catched by lexer
        tokens.Add(Parser.token.END_OF_INPUT)
        tokens
    let getNextToken (tokens : List<Parser.token>) i =
        if tokens.Count = 0 then
            failwith "trying to get token from empty buffer"
        else
            let t = tokens.[0]
            tokens.RemoveAt(0)
            t

let parseString (s : string) =
    let lexbuf = LexBuffer<char>.FromString s
    let tokens = Lexer.lex s
    printfn "tokens: %A" tokens
    Parser.start_entry (Lexer.getNextToken tokens) lexbuf

//parseString "noget"
let prg = parseString "select 'test' as d;"
printfn "%A\n" (prg)
