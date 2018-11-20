namespace MbSqlDriver
module Main =
    open Microsoft.FSharp.Text.Lexing
    let private syntax_error_at at =
        sprintf "You have an error in your SQL syntax; check the manual that corresponds to your MariaDB server version for the right syntax to use near '%s' at line 1" at
    let private parse_string (s : string) =
        let lexbuf = LexBuffer<char>.FromString s
        let (tokens,msg) =
            try (MbSqlLexer.lex s,"") with
            | Failure error_t ->
                ([||],syntax_error_at error_t)
        if msg <> "" then
            [MbSqlAbSyn.Qs.Error msg]
        else
            try MbSqlParser.start_entry (MbSqlLexer.getNextToken tokens) lexbuf with
            | _ ->
                let msg = syntax_error_at (MbSqlLexer.getPrevTokenVal tokens)
                [MbSqlAbSyn.Qs.Error msg]
    let private d2s d =
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
        | 11 -> s+s+s+s+s+s+s+s+s+s+s
        | 12 -> s+s+s+s+s+s+s+s+s+s+s+s
        | 13 -> s+s+s+s+s+s+s+s+s+s+s+s+s
        | 14 -> s+s+s+s+s+s+s+s+s+s+s+s+s+s
        | 15 -> s+s+s+s+s+s+s+s+s+s+s+s+s+s+s
        | 16 -> s+s+s+s+s+s+s+s+s+s+s+s+s+s+s+s
        | n -> exec "" n
    type Traverse<'T> =
        static member gen (f,acc,l) =
            let val_name n = {vname=n;vtype="";vval="";vargs=[||]} : MbSqlTraverse.TreeVal<'T>
            let val_name_type n t = {vname=n;vtype=t;vval="";vargs=[||]} : MbSqlTraverse.TreeVal<'T>
            let val_name_type_args n t args = {vname=n;vtype=t;vval="";vargs=args} : MbSqlTraverse.TreeVal<'T>
            let val_name_type_v n t v = {vname=n;vtype=t;vval=v;vargs=[||]} : MbSqlTraverse.TreeVal<'T>
            let val_name_type_v_args n t v args = {vname=n;vtype=t;vval=v;vargs=args} : MbSqlTraverse.TreeVal<'T>
            let fs = (val_name,val_name_type,val_name_type_args,val_name_type_v,val_name_type_v_args)
            MbSqlTraverse.traverse f acc fs l
    let query2absyn_string (q) =
        let travF depth acc (x : MbSqlTraverse.TreeVal<string>) =
            let d0 = d2s depth
            if x.vname = "options" then
                Array.fold (fun acc x -> acc + x) "" x.vargs
            elif x.vtype = "qoption" then
                let s1 = sprintf "%s%s<option>\n" d0 x.vname
                let s2 = x.vargs.[0]
                acc + s1 + s2
            elif x.vname = "union" then
                let s0 = sprintf "%sunion<%s>\n" d0 x.vtype
                let s1 = sprintf "%s%s" (d2s (depth + 1)) x.vargs.[0]
                let s2 = sprintf "%s%s" (d2s (depth + 1)) x.vargs.[1]
                s0 + s1 + s2
            elif x.vname = "select" then
                let s1 = sprintf "%s<%s>\n" x.vname x.vtype
                let s2 = sprintf "%sselect.options:\n%s" d0 x.vargs.[0]
                let s3 = sprintf "%sselect.items:\n%s" d0 x.vargs.[1]
                let s4 = sprintf "%sselect.into:\n%s" d0 x.vargs.[2]
                let s5 = sprintf "%sselect.from:\n%s" d0 x.vargs.[3]
                let s6 = sprintf "%sselect.where:\n%s" d0 x.vargs.[4]
                let s7 = sprintf "%sselect.group:\n%s" d0 x.vargs.[5]
                let s8 = sprintf "%sselect.having:\n%s" d0 x.vargs.[6]
                let s9 = sprintf "%sselect.window:\n%s" d0 x.vargs.[7]
                s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9
            elif x.vtype = "unary" then
                let s1 = sprintf "%s%s<unary>\n" (d2s depth) x.vname
                let s2 = x.vargs.[0]
                s1 + s2
            elif x.vtype = "binary" then
                let s1 = sprintf "%s%s<binary>\n" (d2s depth) x.vname
                let s2 = x.vargs.[0]
                let s3 = x.vargs.[1]
                s1 + s2 + s3
            elif x.vtype = "call" then
                let s1 = sprintf "%s%s<call>\n" (d2s depth) x.vname
                let s_id = x.vargs.[0]
                let s_args = x.vargs.[1]
                s1 + s_id + s_args
            elif x.vname = "list" then
                let s1 = sprintf "%slist<%s>\n" (d2s depth) x.vtype
                let s2 = Array.fold (fun acc x -> acc + x) s1 x.vargs
                s2
            elif x.vname = "node" && x.vtype <> "" then
                sprintf "%snode<%s>%s\n" d0 x.vtype x.vval
            elif x.vname = "node" then
                sprintf "%snode<>%s\n" d0 x.vval
            else
                acc
        match parse_string q with
        | [MbSqlAbSyn.Qs.Error msg] -> msg
        | l -> Traverse<string>.gen (travF,"",l)
