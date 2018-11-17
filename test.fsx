#r "../FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
open Microsoft.FSharp.Text.Lexing
open System

#load "absyn.fs"
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
    let print_tokens () =
        printfn "tokens:"
        for t in tokens do
            printfn "%A" (snd t)
        printfn "end tokens-------------"
        printfn ""
    
    
    print_tokens()
    
    
    if msg <> "" then
        [AbSyn.Qs.Error msg]
    else
        try Parser.start_entry (Lexer.getNextToken tokens) lexbuf with
        | _ ->
            let msg = syntax_error_at (Lexer.getPrevTokenVal tokens)
            [AbSyn.Qs.Error msg]

let testPrg prg =
    match parseString prg with
    | [AbSyn.Qs.Error msg] -> msg
    | l -> sprintf "success: %A" l
//parseString "noget"
printfn "\ntests-----------"
//select tests
let prg1 = "select 'test' as d, 2 + 3i * 4 - 5 / 6  as c"
let prg2 = "select 1 = 2 is true as a, 'test' as b from (t1,t2) join t3 on t1.noget = t2.noget where id = 200"
let prg3 =
    "select id from a join b on a.t = b.t"+
    " union "+
    "select id from c join d on c.t = d.some"+
    " order by id desc"+
    " /* here's a comment */ "
let prg4 = "select * from t1"
//
//printfn "test1:\n%s" (testPrg inj2)
AbSyn.traverse (fun x -> x) (parseString prg4)
