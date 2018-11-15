#r "../FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
open Microsoft.FSharp.Text.Lexing
open System

#load "mysql.fs"


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
