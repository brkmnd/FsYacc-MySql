#r "../FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
open Microsoft.FSharp.Text.Lexing
open System

#load "parser.fs"
#load "lexer.fs"


let syntax_error_at at =
    sprintf "You have an error in your SQL syntax; check the manual that corresponds to your MariaDB server version for the right syntax to use near '%s' at line 1" at

let parseString (s : string) =
    let lexbuf = LexBuffer<char>.FromString s
    let (tokens,msg) =
        try (Lexer.lex s,"") with
        | Failure error_t ->
            ([||],syntax_error_at error_t)
    //printfn "tokens: %A" tokens
    if msg <> "" then
        [Parser.AbSyn.Qs.Error msg]
    else
        try Parser.start_entry (Lexer.getNextToken tokens) lexbuf with
        | _ ->
            let msg = syntax_error_at (Lexer.getPrevTokenVal tokens)
            [Parser.AbSyn.Qs.Error msg]

let testPrg prg =
    match parseString prg with
    | [Parser.AbSyn.Qs.Error msg] -> msg
    | l -> sprintf "success: %A" l
//parseString "noget"
printfn "\ntests-----------"
let prg1 = "select 'test' as d, 2 + 3i * 4 - 5 / 6  as c"
let prg2 = "select 1 = 2 is true as a, 'test' as b"
printfn "test1:\n%s" (testPrg prg2)
