//#r "../FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"

#load "lib/FsLexYacc.Runtime.Lexing.fs"
#load "lib/FsLexYacc.Runtime.Parsing.fs"
#load "absyn.fs"
#load "traverse.fs"
#load "parser.fs"
#load "lexer.fs"
#load "MbSqlDriver.fs"

let fopen name =
    let l = System.IO.File.ReadAllLines (name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l

printfn "\ntests-----------"
let prg1 = "select 'test' as d, 2 + 3i * 4 - 5 / 6  as c"
let prg2 = "select 1 = 2 is true as a, 'test' as b from (t1,t2) join t3 on t1.noget = t2.noget where id = 200"
let prg3 =
    "select id from a join b on a.t = b.t"+
    " union "+
    "select id from c join d on c.t = d.some"+
    " order by id desc"+
    " /* here's a comment */ "
let prg4 = "select 1 as a"
let prg5 = "select (1 + 2) * 3 / (4 - 5)"
let prg6 = "select 2 * (3 + 4) - -2 * 2 as i1, i2, true as i3 from t1 join t2"
let prg7 = "select i1 from t2"
let prg8 = "select 1 + 2 + 3"
let prg9 = "select 2 * (3 + 4) - -2 * 2 as i1, i2, true as i3 from t1"
let prg10 = "select 2 * (3 + 4) - -2 * 2 as i1, id, true as i3 from users,blogaden"

//errors
let err1 = "select 1 from t1 join 'test'"

let q = prg10
printfn "q:%s" q
printfn "%s" (MbSqlDriver.query2absyn_string(q))

