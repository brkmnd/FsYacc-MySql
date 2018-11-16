%{
//AbSyn
module AbSyn =
    type Expr =
        | Binary of string * Expr * Expr
        | Unary of string * Expr
        | Node of string
        | NodeTyped of string * string
        | ExprList of Expr list
        | ExprListTyped of string * (Expr list)
        | Function of string * (Expr list)
        | Null
        | Temp
    type Q_Select =
        | SelectNull
        | SelectOptions of Expr list
        | SelectItems of (Expr * Expr) list
        | SelectInto of Expr
        | SelectFrom of Expr list
        | SelectWhere of Expr
        | SelectGroup of Expr
        | SelectHaving of Expr
        | SelectWindow of Expr
        | SelectOrder of Expr
        | SelectLimit of Expr
    type Qs_Option =
        | OptOrder of Expr
        | OptLimit of Expr
        | OptLocking of Expr
    type Qs =
        | Select of Q_Select list
        | Options of Qs * (Qs_Option list)
        | Union of string * Qs * Qs
        | Error of string
        | Null
%}
/*
    Tokens
        I have tried to differ the keywords on key and nokey
        -nokey should be allowed as id - thus include all nokey in ident
        -key should not be allowed unless enclosed in '' or "".
        Keywords are listed here: https://dev.mysql.com/doc/refman/8.0/en/keywords.html
        R/reserved for key
        noreserved for nokey
*/
//delimiters
%token DELIM_SCOLON DELIM_COMMA END_OF_INPUT
//pars
%token PAR_LPAR PAR_RPAR PAR_LBRACE PAR_RBRACE
//operators
%token OP_UNION
%token OP_JOIN OP_INNER OP_CROSS OP_STRAIGHT_JOIN OP_NATURAL OP_LEFT OP_RIGHT OP_ON OP_USING OP_INTO
%token OP_PLUS OP_MINUS OP_DIV OP_TIMES OP_PERC OP_DOT
%token OP_NOT OP_OR OP_XOR OP_AND OP_TILDE OP_LIKE OP_BANG
%token OP_EQ OP_EQ2 OP_NEQ OP_NEQ2 OP_GT OP_LT OP_GEQ OP_LEQ
%token OP_BOR OP_BAND OP_UP OP_SHIFT_LEFT OP_SHIFT_RIGHT
%token OP_IN OP_BETWEEN OP_SOUNDS
%token OP_MOD_TXT OP_DIV_TXT OP_NOT_TXT OP_OR_TXT OP_AND_TXT
%token OP_JSON_TABLE
//not in lexer yet
%token OP_OUTER
%token OP_GROUP
%token OP_AS
//keywords
%token KEY_SELECT
%token KEY_AS
%token KEY_IS
%token KEY_ALL
%token KEY_ANY
%token KEY_OUTFILE
%token KEY_FROM
%token KEY_FORCE
%token KEY_IGNORE
%token KEY_KEY
%token KEY_KEYS
%token KEY_INDEX
%token KEY_INDEXES
%token KEY_UNIQUE
%token KEY_COLUMNS
%token KEY_FOR
%token KEY_WHERE
%token KEY_ORDER
%token KEY_BY
%token KEY_LIMIT
%token KEY_ASC
%token KEY_DESC
%token KEY_OFFSET
//not in lexer yet
%token KEY_PARTITION
%token KEY_DISTINCT
%token KEY_EXISTS
%token KEY_DEFAULT
%token KEY_USE
%token KEY_PRIMARY
%token KEY_HIGH_PRIORITY
%token KEY_SQL_SMALL_RESULT
%token KEY_SQL_BIG_RESULT
%token KEY_SQL_BUFFER_RESULT
%token KEY_SQL_CALC_FOUND_ROWS
%token KEY_ROLLUP
%token KEY_HAVING
%token KEY_WINDOW
//might be deprecated
%token KEY_SQL_NO_CACHE
//nokeys are not reserverd keywords. these can be used for idents 
%token NOKEY_ORDINALITY
%token NOKEY_PATH
%token NOKEY_NESTED
//values
%token<string>VAL_ID
%token<string>VAL_NUM
%token<string>VAL_HEX
%token<string>VAL_BIN
%token<string>VAL_STRING
%token VAL_NULL
%token VAL_TRUE
%token VAL_FALSE
%token VAL_UNKNOWN
%token VAL_DUAL
//not in lexer yet
%token VAL_BOOL
%token VAL_EMPTY
%token VAL_ERROR

%left   PREC_CONDITIONLESS_JOIN
%left   OP_JOIN OP_INNER OP_CROSS OP_STRAIGHT_JOIN OP_NATURAL OP_LEFT OP_RIGHT OP_ON OP_USING
%left   OP_OR OP_OR_TXT OP_BOR
%left   OP_XOR
%left   OP_AND OP_AND_TXT
%left   OP_BETWEEN KEY_CASE KEY_WHEN KEY_THEN KEY_ELSE
%left   OP_EQ OP_EQ2 OP_GEQ OP_GT OP_LEQ OP_LT OP_NEQ OP_IS OP_LIKE OP_REGEXP OP_IN
%left   OP_LOR
%left   OP_LAND
%left   OP_SHIFT_LEFT OP_SHIFT_RIGHT
%left   OP_MINUS OP_PLUS
%left   OP_TIMES OP_DIV OP_PERC OP_DIV_TXT OP_MOD_TXT
%left   OP_UP
%left   OP_TILDE NEG
%right  OP_NOT OP_NOT_TXT
%right  BINARY_SYM COLLATE_SYM
%left  INTERVAL_SYM
%left PREC_SUBQUERY_AS_EXPR
%left PAR_LPAR PAR_RPAR

%left PREC_EMPTY_FROM_CLAUSE
%right OP_INTO

%start start_entry
%type<AbSyn.Qs list> start_entry

%%
/*
    Parses to a structure of types given in AbSyn.
    The parser can be stopped with failwith "whatever"
    - most likely "syntax error". Then a syntax error
    will be returned as given in mysql.fs 
*/
start_entry:
          sql_statement { $1 }
          /* Missing an array of GRAMMAR_SELECTOR expressions */
        ;

sql_statement:
          END_OF_INPUT { [] }
        | simple_statement_or_begin DELIM_SCOLON opt_end_of_input {
            //we found a wellformed query...
            //join the queries parser-wise instead of inside the action
            //as done in the original
            [$1] @ $3
            }
        | simple_statement_or_begin END_OF_INPUT {
            //single query, not terminated.
            [$1]
            }
        ;
opt_end_of_input:
        /* empty */         { [] }
        | sql_statement     { $1 }
        ;
simple_statement_or_begin:
          simple_statement  { $1 }
        //| begin_stmt
        ;

simple_statement:
        /* Not implemented yet has been commented out. */
        //  alter_database_stmt           { $$= nullptr; }
        //| alter_event_stmt              { $$= nullptr; }
        //| alter_function_stmt           { $$= nullptr; }
        //| alter_instance_stmt
        //| alter_logfile_stmt            { $$= nullptr; }
        //| alter_procedure_stmt          { $$= nullptr; }
        //| alter_resource_group_stmt
        //| alter_server_stmt             { $$= nullptr; }
        //| alter_tablespace_stmt         { $$= nullptr; }
        //| alter_table_stmt
        //| alter_user_stmt               { $$= nullptr; }
        //| alter_view_stmt               { $$= nullptr; }
        //| analyze_table_stmt
        //| binlog_base64_event           { $$= nullptr; }
        //| call_stmt
        //| change                        { $$= nullptr; }
        //| check_table_stmt
        //| checksum                      { $$= nullptr; }
        //| clone_stmt                    { $$= nullptr; }
        //| commit                        { $$= nullptr; }
        //| create                        { $$= nullptr; }
        //| create_index_stmt
        //| create_resource_group_stmt
        //| create_role_stmt
        //| create_srs_stmt
        //| create_table_stmt
        //| deallocate                    { $$= nullptr; }
        //| delete_stmt
        //| describe_stmt
        //| do_stmt
        //| drop_database_stmt            { $$= nullptr; }
        //| drop_event_stmt               { $$= nullptr; }
        //| drop_function_stmt            { $$= nullptr; }
        //| drop_index_stmt
        //| drop_logfile_stmt             { $$= nullptr; }
        //| drop_procedure_stmt           { $$= nullptr; }
        //| drop_resource_group_stmt
        //| drop_role_stmt
        //| drop_server_stmt              { $$= nullptr; }
        //| drop_srs_stmt
        //| drop_tablespace_stmt          { $$= nullptr; }
        //| drop_table_stmt               { $$= nullptr; }
        //| drop_trigger_stmt             { $$= nullptr; }
        //| drop_user_stmt                { $$= nullptr; }
        //| drop_view_stmt                { $$= nullptr; }
        //| execute                       { $$= nullptr; }
        //| explain_stmt
        //| flush                         { $$= nullptr; }
        //| get_diagnostics               { $$= nullptr; }
        //| group_replication             { $$= nullptr; }
        //| grant                         { $$= nullptr; }
        //| handler_stmt
        //| help                          { $$= nullptr; }
        //| import_stmt                   { $$= nullptr; }
        //| insert_stmt
        //| install                       { $$= nullptr; }
        //| kill                          { $$= nullptr; }
        //| load_stmt
        //| lock                          { $$= nullptr; }
        //| optimize_table_stmt
        //| keycache_stmt
        //| preload_stmt
        //| prepare                       { $$= nullptr; }
        //| purge                         { $$= nullptr; }
        //| release                       { $$= nullptr; }
        //| rename                        { $$= nullptr; }
        //| repair_table_stmt
        //| replace_stmt
        //| reset                         { $$= nullptr; }
        //| resignal_stmt                 { $$= nullptr; }
        //| restart_server_stmt
        //| revoke                        { $$= nullptr; }
       //| rollback                      { $$= nullptr; }
       //| savepoint                     { $$= nullptr; }
       | select_stmt                    { $1 }
       //| set                           { $$= nullptr; CONTEXTUALIZE($1); }
       //| set_resource_group_stmt
       //| set_role_stmt
       //| signal_stmt                   { $$= nullptr; }
       //| show                          { $$= nullptr; }
       //| shutdown_stmt
       //| slave                         { $$= nullptr; }
       //| start                         { $$= nullptr; }
       //| truncate_stmt
       //| uninstall                     { $$= nullptr; }
       //| unlock                        { $$= nullptr; }
        //| update_stmt
       //| use                           { $$= nullptr; }
       //| xa                            { $$= nullptr; }
         ;
/*
        Statements
*/
/* Start of SELECT statement */
select_stmt:
          query_expression          { $1 }
        | query_expression_parens   { $1 }
        //| select_stmt_with_into {}
        ;
row_subquery:
          subquery          { AbSyn.Qs.Null }
        ;

table_subquery:
          subquery          { AbSyn.Qs.Null }
        ;

subquery:
          query_expression_parens %prec PREC_SUBQUERY_AS_EXPR {
            AbSyn.Qs.Null
            }
        ;
query_expression:
          query_expression_body
          opt_order_clause
          opt_limit_clause
          opt_locking_clause_list {
          let options = [
            AbSyn.Qs_Option.OptOrder $2
            AbSyn.Qs_Option.OptLimit $3
            AbSyn.Qs_Option.OptLocking $4
            ]
          AbSyn.Qs.Options ($1,options)
          }
        /*
        | with_clause
          query_expression_body
          opt_order_clause
          opt_limit_clause
          opt_locking_clause_list {
            $$= NEW_PTN PT_query_expression($1, $2, $3, $4, $5);
            }
        | query_expression_parens
          order_clause
          opt_limit_clause
          opt_locking_clause_list {
            auto nested= NEW_PTN PT_nested_query_expression($1);
            auto body= NEW_PTN PT_query_expression_body_primary(nested);
            $$= NEW_PTN PT_query_expression(body, $2, $3, $4);
            r}
        | with_clause
          query_expression_parens
          order_clause
          opt_limit_clause
          opt_locking_clause_list {
            auto nested= NEW_PTN PT_nested_query_expression($2);
            auto body= NEW_PTN PT_query_expression_body_primary(nested);
            $$= NEW_PTN PT_query_expression($1, body, $3, $4, $5);
            }
        | query_expression_parens
          limit_clause
          opt_locking_clause_list {
            if ($1 == NULL)
              MYSQL_YYABORT; // OOM
            $$= NEW_PTN PT_query_expression($1->body(), NULL, $2, $3);
            }
        | with_clause
          query_expression_parens
          limit_clause
          opt_locking_clause_list {
            if ($2 == NULL)
              MYSQL_YYABORT; // OOM
            $$= NEW_PTN PT_query_expression($1, $2->body(), NULL, $3, $4);
            }
        | with_clause
          query_expression_parens
          opt_locking_clause_list {
            if ($2 == NULL)
              MYSQL_YYABORT; // OOM
            $$= NEW_PTN PT_query_expression($1, $2->body(), NULL, NULL, $3);
            }
        */
        ;
/* order, limit, locking */
opt_order_clause:
          /* empty */                           { AbSyn.Expr.Null }
        | order_clause                          { AbSyn.Expr.ExprList $1 }
        ;
order_clause:
          KEY_ORDER KEY_BY order_list           { $3 }
        ;
order_list:
          order_list DELIM_COMMA order_expr     { $1 @ [$3] }
        | order_expr                            { [$1] }
        ;
order_expr:
          expr opt_ordering_direction {
            AbSyn.Expr.Unary ($2,$1)
            }
        ;
opt_ordering_direction:
          /* empty */           { "asc" }
        | ordering_direction    { $1 }
        ;
ordering_direction:
          KEY_ASC               { "asc" }
        | KEY_DESC              { "desc" }
        ;
opt_limit_clause:
          /* empty */               { AbSyn.Expr.Null }
        | limit_clause              { AbSyn.Expr.ExprList $1 }
        ;
limit_clause:
          KEY_LIMIT limit_options   { $2 }
        ;
limit_options:
          limit_option                          { [$1] }
        | limit_option DELIM_COMMA limit_option { [$1;$3] }
        | limit_option KEY_OFFSET limit_option  { [$1;$3] }
        ;
limit_option:
          ident             { $1 }
        //| param_marker      {}
        //| ULONGLONG_NUM     {}
        //| LONG_NUM          {}
        | VAL_NUM           { AbSyn.Expr.NodeTyped ("num",$1) }
        ;
opt_simple_limit:
          /* empty */               { AbSyn.Expr.Temp }
        | KEY_LIMIT limit_option    { AbSyn.Expr.Temp }
        ;
opt_locking_clause_list:
          /* Empty */           { AbSyn.Expr.Null }
        | locking_clause_list   { AbSyn.Expr.ExprList $1 }
        ;

locking_clause_list:
          locking_clause_list locking_clause    { $1 @ [$2] }
        | locking_clause                        { [$1] }
        ;

locking_clause:
           KEY_FOR { AbSyn.Expr.Temp }
        //  KEY_FOR lock_strength opt_locked_row_action
        //| KEY_FOR lock_strength table_locking_list opt_locked_row_action
        //| LOCK_SYM IN_SYM SHARE_SYM MODE_SYM
        ;

query_expression_body:
          query_primary { $1 }
        | query_expression_body OP_UNION union_option query_primary {
            AbSyn.Qs.Union ($3,$1,$4)
            }
        | query_expression_parens OP_UNION union_option query_primary {
            AbSyn.Qs.Union ($3,$1,$4)
            }
        | query_expression_body OP_UNION union_option 
          query_expression_parens {
            AbSyn.Qs.Union ($3,$1,$4)
            }
        | query_expression_parens OP_UNION union_option 
          query_expression_parens {
            AbSyn.Qs.Union ($3,$1,$4)
            }
        ;
union_option:
          /* empty */       { "none" }
        | KEY_DISTINCT      { "distinct" }
        | KEY_ALL           { "all" }
        ;
query_expression_parens:
          PAR_LPAR query_expression_parens PAR_RPAR { $2 }
        | PAR_LPAR query_expression PAR_RPAR        { $2 }
        ;
query_primary:
          query_specification                       { $1 }
        ;

query_specification:
          KEY_SELECT
          select_options
          select_item_list
          into_clause
          opt_from_clause
          opt_where_clause
          opt_group_clause
          opt_having_clause
          opt_window_clause {
            AbSyn.Qs.Select [
                AbSyn.Q_Select.SelectOptions $2
                AbSyn.Q_Select.SelectItems $3
                AbSyn.Q_Select.SelectInto $4
                AbSyn.Q_Select.SelectFrom $5
                AbSyn.Q_Select.SelectWhere $6
                AbSyn.Q_Select.SelectGroup $7
                AbSyn.Q_Select.SelectHaving $8
                AbSyn.Q_Select.SelectWindow $9
                ]
            }
        | KEY_SELECT
          select_options
          select_item_list
          opt_from_clause
          opt_where_clause
          opt_group_clause
          opt_having_clause
          opt_window_clause {
            //same as above but no into
            AbSyn.Qs.Select [
                AbSyn.Q_Select.SelectOptions $2
                AbSyn.Q_Select.SelectItems $3
                AbSyn.Q_Select.SelectInto (AbSyn.Expr.Null)
                AbSyn.Q_Select.SelectFrom $4
                AbSyn.Q_Select.SelectWhere $5
                AbSyn.Q_Select.SelectGroup $6
                AbSyn.Q_Select.SelectHaving $7
                AbSyn.Q_Select.SelectWindow $8
                ]
            }
        ;
select_options:
          /* empty*/            { [] }
        | select_option_list    { $1 }
        ;
select_option_list:
          select_option_list select_option  { $1 @ [$2]}
        | select_option                     { [$1] }
        ;
select_option:
          query_spec_option    { $1 }
        | KEY_SQL_NO_CACHE {
            //might be deprecated
            AbSyn.Expr.NodeTyped ("option","sql no cache")
            }
        ;
query_spec_option:
          OP_STRAIGHT_JOIN {
            AbSyn.Expr.NodeTyped ("option","straight join")
            }
        | KEY_HIGH_PRIORITY {
            AbSyn.Expr.NodeTyped ("option","high priority")
            }
        | KEY_DISTINCT {
            AbSyn.Expr.NodeTyped ("option","distinct")
            }
        | KEY_SQL_SMALL_RESULT {
            AbSyn.Expr.NodeTyped ("option","small result")
            }
        | KEY_SQL_BIG_RESULT {
            AbSyn.Expr.NodeTyped ("option","big result")
            }
        | KEY_SQL_BUFFER_RESULT {
            AbSyn.Expr.NodeTyped ("option","buffer result")
            }
        | KEY_SQL_CALC_FOUND_ROWS {
            AbSyn.Expr.NodeTyped ("option","calc found rows")
            }
        | KEY_ALL {
            AbSyn.Expr.NodeTyped ("option","all")
            }
        ;
select_item_list:
          select_item_list DELIM_COMMA select_item {
            $1 @ [$3]
            }
        | select_item {
            [$1]
            }
        | OP_TIMES {
            [(AbSyn.Expr.Node "*",AbSyn.Expr.Null)]
            }
        ;

select_item:
          table_wild {
            ($1,AbSyn.Expr.Null)
            }
        | expr select_alias {
            //$$= NEW_PTN PTI_expr_with_alias(@$, $1, @1.cpp, $2);
            ($1,$2)
            }
        ;
select_alias:
          /* empty */       { AbSyn.Expr.Null }
        | KEY_AS ident      { $2 }
        | KEY_AS VAL_STRING { AbSyn.Expr.Temp }
        | ident             { $1 }
        | VAL_STRING        { AbSyn.Expr.Temp }
        ;
/* Start of into clause */
into_clause:
          OP_INTO into_destination { $2 }
        ;
into_destination:
        KEY_OUTFILE { AbSyn.Expr.Temp }
        /*
          KEY_OUTFILE TEXT_STRING_filesystem
          opt_load_data_charset
          opt_field_term opt_line_term
          {
            $$= NEW_PTN PT_into_destination_outfile(@$, $2, $3, $4, $5);
          }
        | DUMPFILE TEXT_STRING_filesystem
          {
            $$= NEW_PTN PT_into_destination_dumpfile(@$, $2);
          }
        | select_var_list { $$= $1; }
        */
        ;
/* End of into clause */
/* Start of from clause - this contains join as well and is quite extensive */
opt_from_clause:
          /* Empty. */ %prec PREC_EMPTY_FROM_CLAUSE {
            [AbSyn.Expr.Null]
            }
        | from_clause {
            $1
            }
        ;
from_clause:
          KEY_FROM from_tables { $2 }
        ;

from_tables:
          VAL_DUAL {
            //dual is dummy for no table
            [AbSyn.Expr.Null]
            }
        | table_reference_list { $1 }
        ;

table_reference_list:
          table_reference {
            [$1]
            }
        | table_reference_list DELIM_COMMA table_reference {
            $1 @ [$3]
            }
        ;
table_reference:
          table_factor { $1 }
        | joined_table { $1 }
        //| PAR_LBRACE ident esc_table_reference PAR_RBRACE { $3 }
        ;
joined_table:
          table_reference inner_join_type table_reference OP_ON expr {
            AbSyn.Expr.Binary ($2,$1,AbSyn.Expr.Binary("on",$3,$5))
            }
        | table_reference inner_join_type table_reference OP_USING
          PAR_LPAR using_list PAR_RPAR {
            AbSyn.Expr.Binary ($2,$1,AbSyn.Expr.Binary("using",$3,$6))
            }
        | table_reference outer_join_type table_reference OP_ON expr {
            AbSyn.Expr.Binary ($2,$1,AbSyn.Expr.Binary("on",$3,$5))
            }
        | table_reference outer_join_type table_reference OP_USING
          PAR_LPAR using_list PAR_RPAR {
            AbSyn.Expr.Binary ($2,$1,AbSyn.Expr.Binary("using",$3,$6))
            }
        | table_reference inner_join_type table_reference
          %prec PREC_CONDITIONLESS_JOIN {
            AbSyn.Expr.Binary ($2,$1,$3)
            }
        | table_reference natural_join_type table_factor {
            AbSyn.Expr.Binary ($2,$1,$3)
            }
        ;
/* opts below return join-types to concat with above in expression names */
natural_join_type:
          OP_NATURAL opt_inner OP_JOIN          { "natural"+$2+" join" }
        | OP_NATURAL OP_RIGHT opt_outer OP_JOIN { "natural right"+$3+" join"}
        | OP_NATURAL OP_LEFT opt_outer OP_JOIN  { "natural left"+$3+" join"}
        ;
inner_join_type:
          OP_JOIN                           { "join" }
        | OP_INNER OP_JOIN                  { "inner join" }
        | OP_CROSS OP_JOIN                  { "cross join" }
        | OP_STRAIGHT_JOIN                  { "straight_join" }

outer_join_type:
          OP_LEFT opt_outer OP_JOIN         { "left"+$2+" join" }
        | OP_RIGHT opt_outer OP_JOIN        { "right"+$2+" join" }
        ;

opt_inner:
          /* empty */   { "" }
        | OP_INNER      { " inner" }
        ;

opt_outer:
          /* empty */   { "" }
        | OP_OUTER      { " outer" }
        ;
/*
  table PARTITION (list of partitions), reusing using_list instead of creating
  a new rule for partition_list.
*/
opt_use_partition:
          /* empty */       { AbSyn.Expr.Temp }
        | use_partition     { AbSyn.Expr.Temp }
        ;

use_partition:
          KEY_PARTITION PAR_LPAR using_list PAR_RPAR {
            $3
            }
        ;

/**
  MySQL has a syntax extension where a comma-separated list of table
  references is allowed as a table reference in itself, for instance

    SELECT * FROM (t1, t2) JOIN t3 ON 1

  which is not allowed in standard SQL. The syntax is equivalent to

    SELECT * FROM (t1 CROSS JOIN t2) JOIN t3 ON 1

  We call this rule table_reference_list_parens.

  A <table_factor> may be a <single_table>, a <subquery>, a <derived_table>, a
  <joined_table>, or the bespoke <table_reference_list_parens>, each of those
  enclosed in any number of parentheses. This makes for an ambiguous grammar
  since a <table_factor> may also be enclosed in parentheses. We get around
  this by designing the grammar so that a <table_factor> does not have
  parentheses, but all the sub-cases of it have their own parentheses-rules,
  i.e. <single_table_parens>, <joined_table_parens> and
  <table_reference_list_parens>. It's a bit tedious but the grammar is
  unambiguous and doesn't have shift/reduce conflicts.
*/

/*As of the above. Translate (t1,t2,...) into an expression list */
table_factor:
          single_table                  { $1 }
        | single_table_parens           { $1 }
        | derived_table                 { $1 }
        | joined_table_parens           { $1 }
        | table_reference_list_parens   { AbSyn.Expr.ExprList $1 }
        | table_function                { $1 }
        ;
table_reference_list_parens:
          PAR_LPAR table_reference_list_parens PAR_RPAR {
            $2
            }
        | PAR_LPAR table_reference_list DELIM_COMMA table_reference PAR_RPAR {
            $2 @ [$4]
            }
        ;
single_table_parens:
          PAR_LPAR single_table_parens PAR_RPAR { $2 }
        | PAR_LPAR single_table PAR_RPAR        { $2 }
        ;

single_table:
          table_ident opt_use_partition opt_table_alias opt_key_definition {
            //$3 is ExprId -> ExpExprAlias 
            AbSyn.Expr.ExprListTyped ("id",[$1|>$3;$4])
            }
        ;

joined_table_parens:
          PAR_LPAR joined_table_parens PAR_RPAR { $2 }
        | PAR_LPAR joined_table PAR_RPAR        { $2 }
        ;

derived_table:
          table_subquery opt_table_alias opt_derived_column_list {
            AbSyn.Expr.Temp
            }
        ;
opt_derived_column_list:
          /* empty */ {
            AbSyn.Expr.Temp
            }
        | PAR_LPAR simple_ident_list PAR_RPAR {
            AbSyn.Expr.Temp
            }
        ;
simple_ident_list:
          ident                                 { [$1] }
        | simple_ident_list DELIM_COMMA ident   { $1 @ [$3] }
        ;
table_function:
          OP_JSON_TABLE PAR_LPAR expr DELIM_COMMA
          text_string_sys columns_clause PAR_RPAR
          opt_table_alias {
            // Alias isn't optional, follow derived's behavior
            //Not sure what this is, so leave as temp for now
            AbSyn.Expr.Temp
            }
        ;

columns_clause:
          KEY_COLUMNS PAR_LPAR columns_list PAR_RPAR { $3 }
        ;

columns_list:
          jt_column                             { [$1] }
        | columns_list DELIM_COMMA jt_column    { $1 @ [$3] }
        ;

jt_column:
          ident KEY_FOR NOKEY_ORDINALITY {
            AbSyn.Expr.Temp
            }
        | ident set_type jt_column_type NOKEY_PATH text_string_sys
          opt_on_empty_or_error {
            AbSyn.Expr.Temp
            }
        | NOKEY_NESTED NOKEY_PATH text_string_sys columns_clause {
            AbSyn.Expr.Temp
            }
        ;

jt_column_type:
          /* empty */   { AbSyn.Expr.Temp }
        | KEY_EXISTS    { AbSyn.Expr.Temp }
        ;

opt_on_empty_or_error:
          /* empty */               { AbSyn.Expr.Temp }
        | opt_on_empty              { AbSyn.Expr.Temp }
        | opt_on_error              { AbSyn.Expr.Temp }
        | opt_on_empty opt_on_error { AbSyn.Expr.Temp }
        | opt_on_error opt_on_empty { AbSyn.Expr.Temp }
        ;

opt_on_empty:
          jt_on_response OP_ON VAL_EMPTY       { AbSyn.Expr.Temp }
        ;
opt_on_error:
          jt_on_response OP_ON VAL_ERROR       { AbSyn.Expr.Temp }
        ;
jt_on_response:
          VAL_ERROR                     { AbSyn.Expr.Temp }
        | VAL_NULL                      { AbSyn.Expr.Temp }
        | KEY_DEFAULT text_string_sys   { AbSyn.Expr.Temp }
        ;
index_hint_clause:
          /* empty */               { AbSyn.Expr.Temp }
        | KEY_FOR OP_JOIN           { AbSyn.Expr.Temp }
        | KEY_FOR KEY_ORDER KEY_BY  { AbSyn.Expr.Temp }
        | KEY_FOR OP_GROUP KEY_BY   { AbSyn.Expr.Temp }
        ;
index_hint_type:
          KEY_FORCE  { "force" }
        | KEY_IGNORE { "ignore" }
        ;
index_hint_definition:
          index_hint_type key_or_index index_hint_clause
          PAR_LPAR key_usage_list PAR_RPAR {
            AbSyn.Expr.Temp 
            }
        | KEY_USE key_or_index index_hint_clause
          PAR_LPAR opt_key_usage_list PAR_RPAR {
            AbSyn.Expr.Temp 
            }
       ;
index_hints_list:
          index_hint_definition { [] }
        | index_hints_list index_hint_definition {
            []
            }
        ;
opt_index_hints_list:
          /* empty */           { [] }
        | index_hints_list      { $1 }
        ;
/* End of from clause */

/* Start of where clause */
opt_where_clause:
        opt_where_clause_expr { $1 }
        ;
opt_where_clause_expr:
        /* empty */  { AbSyn.Expr.Null }
        | KEY_WHERE expr { $2 }
        ;
/* End of where clause */
opt_key_definition:
          opt_index_hints_list  { AbSyn.Expr.Temp }
        ;
/* Start of group clause */
/*
grouping_expr was present to return a different type,
but the production was just an expression. Has been skipped.
*/
opt_group_clause:
          /* empty */                           { AbSyn.Expr.Null }
        | OP_GROUP KEY_BY group_list olap_opt   {
            AbSyn.Expr.ExprListTyped ($4,$3)
            }
        ;
group_list:
          group_list DELIM_COMMA expr       { $1 @ [$3] }
        | expr                              { [$1] }
        ;
olap_opt:
          /* empty */   { "no-rollup" }
        | KEY_ROLLUP    { "rollup" }
            /*
              'WITH ROLLUP' is needed for backward compatibility,
              and cause LALR(2) conflicts.
              This syntax is not standard.
              MySQL syntax: GROUP BY col1, col2, col3 WITH ROLLUP
              SQL-2003: GROUP BY ... ROLLUP(col1, col2, col3)
              edit: have skipped the "WITH ROLLUP"
            */
        ;
/* End of group clause */
/* Start of having clause */
opt_having_clause:
          /* empty */       { AbSyn.Expr.Null }
        | KEY_HAVING expr   { $2 }
        ;
/* End of having clause */
/* Start of window clause */
opt_window_clause:
          /* Nothing */                     { AbSyn.Expr.Null }
        | KEY_WINDOW window_definition_list { AbSyn.Expr.ExprList $2 }
        ;

window_definition_list:
          window_definition                                     { [$1] }
        | window_definition_list DELIM_COMMA window_definition  { $1 @ [$3]}
        ;

window_definition:
          window_name KEY_AS window_spec {
            AbSyn.Expr.Binary ("as",$1,$3)
            }
        ;
window_name:
          ident         { $1 }
        ;
window_spec:
          /* spec is left as. not completed */
          PAR_LPAR /* window_spec_details */ PAR_RPAR { AbSyn.Expr.Null }
        ;
/* End of window clause */
opt_key_usage_list:
          /* empty */       { AbSyn.Expr.Temp }
        | key_usage_list    { AbSyn.Expr.Temp }
        ;

key_usage_element:
          ident             { AbSyn.Expr.Temp }
        | KEY_PRIMARY       { AbSyn.Expr.Temp }
        ;

key_usage_list:
          key_usage_element                             { AbSyn.Expr.Temp }
        | key_usage_list DELIM_COMMA key_usage_element  { AbSyn.Expr.Temp }
        ;

using_list:
          ident_string_list { AbSyn.Expr.ExprListTyped ("id",$1) }
        ;

ident_string_list:
          ident                                 { [$1] }
        | ident_string_list DELIM_COMMA ident   { $1 @ [$3] }
        ;
opt_as_or_eq:
          /* empty */   { "" }
        | OP_AS         { "as" }
        | OP_EQ         { "eq" }
        ;
opt_table_alias:
          /* empty */        {
            fun tid ->
                AbSyn.Expr.Binary (
                    "as",
                    tid,
                    AbSyn.Expr.Null
                    )
            }
        | opt_as_or_eq ident {
            fun tid ->
                AbSyn.Expr.Binary (
                    $1,
                    tid,
                    $2
                    )
            }
        ;
opt_all:
          /* empty */   { "" }
        | KEY_ALL       { "all" }
        ;
key_or_index:
          KEY_KEY   { "key" }
        | KEY_INDEX { "index" }
        ;

opt_key_or_index:
          /* empty */   { "" }
        | key_or_index  { $1 }
        ;
keys_or_index:
          KEY_KEYS      { "keys" }
        | KEY_INDEX     { "index" }
        | KEY_INDEXES   { "indexes" }
        ;
opt_unique:
          /* empty */  { "" }
        | KEY_UNIQUE   { "unique" }
        ;
/* End of SELECT statement */
/* Start of INSERT statement */
/* End of INSERT statement */
/*
        Types
*/
set_type:
        VAL_BOOL        { AbSyn.Expr.Temp }
/*
        Expressions
*/
expr_list:
          expr                          { $1 }
        | expr_list DELIM_COMMA expr    { $3 }
        ;
expr:
          expr op_or expr %prec OP_OR {
            AbSyn.Expr.Binary ("or",$1,$3)
            }
        | expr OP_XOR expr %prec OP_XOR {
            AbSyn.Expr.Binary ("xor",$1,$3)
            }
        | expr op_and expr %prec OP_AND {
            AbSyn.Expr.Binary ("and",$1,$3)
            }
        | OP_NOT expr %prec OP_NOT {
            AbSyn.Expr.Unary ("not",$2)
            }
        | bool_pri KEY_IS VAL_TRUE %prec KEY_IS {
            AbSyn.Expr.Binary ("is",$1,AbSyn.Expr.NodeTyped ("bool","true"))
            }
        | bool_pri KEY_IS op_not VAL_TRUE %prec KEY_IS {
            //Do it non reversible - that is cannot map back since 
            //not true == false
            AbSyn.Expr.Binary ("is",$1,AbSyn.Expr.NodeTyped ("bool","false"))
            }
        | bool_pri KEY_IS VAL_FALSE %prec KEY_IS {
            AbSyn.Expr.Binary ("is",$1,AbSyn.Expr.NodeTyped ("bool","false"))
            }
        | bool_pri KEY_IS op_not VAL_FALSE %prec KEY_IS {
            //Do it non reversible - that is cannot map back. As above
            AbSyn.Expr.Binary ("is",$1,AbSyn.Expr.NodeTyped ("bool","true"))
            }
        | bool_pri KEY_IS VAL_UNKNOWN %prec KEY_IS {
            //I cant quite figure what unknown is. So treat as key
            AbSyn.Expr.Binary ("is",$1,AbSyn.Expr.NodeTyped ("key","unknown"))
            }
        | bool_pri KEY_IS op_not VAL_UNKNOWN %prec KEY_IS {
            //The negation of unknown here just become known
            AbSyn.Expr.Binary ("is",$1,AbSyn.Expr.NodeTyped ("key","known"))
            }
        | bool_pri {
            $1
            }
        ;
bool_pri:
          bool_pri KEY_IS VAL_NULL %prec KEY_IS {
            AbSyn.Expr.Binary ("is",$1,AbSyn.Expr.NodeTyped ("key","null"))
            }
        | bool_pri KEY_IS op_not VAL_NULL %prec KEY_IS {
            //Done the long way
            AbSyn.Expr.Binary ("is",$1,AbSyn.Expr.Unary ("not",AbSyn.Expr.NodeTyped("key","null")))
          }
        | bool_pri comp_op predicate {
            AbSyn.Expr.Binary ($2,$1,$3)
            }
        //| bool_pri comp_op all_or_any table_subquery %prec OP_EQ {}
        | predicate {
            $1
            }
        ;
predicate:
        //  bit_expr OP_IN table_subquery {}
        //| bit_expr op_not OP_IN table_subquery {}
        | bit_expr OP_IN PAR_LPAR expr PAR_RPAR {
            AbSyn.Expr.Binary ("in",$1,$4)
            }
        | bit_expr OP_IN PAR_LPAR expr DELIM_COMMA expr_list PAR_RPAR {
            AbSyn.Expr.Temp
            }
        | bit_expr op_not OP_IN PAR_LPAR expr PAR_RPAR {
            AbSyn.Expr.Unary ("not",AbSyn.Expr.Binary ("in",$1,$5))
            }
        | bit_expr op_not OP_IN PAR_LPAR expr DELIM_COMMA expr_list PAR_RPAR {
            AbSyn.Expr.Temp
            }
        | bit_expr OP_BETWEEN bit_expr OP_AND predicate {
            AbSyn.Expr.Temp
            }
        | bit_expr op_not OP_BETWEEN bit_expr OP_AND predicate {
            AbSyn.Expr.Temp
            }
        | bit_expr OP_SOUNDS OP_LIKE bit_expr {
            AbSyn.Expr.Temp
            }
        //| bit_expr OP_LIKE simple_expr opt_escape {}
        //| bit_expr op_not OP_LIKE simple_expr opt_escape {}
        //| bit_expr KEY_REGEXP bit_expr {}
        //| bit_expr not KEY_REGEXP bit_expr {}
        | bit_expr {
            $1
            }
        ;

bit_expr:
          bit_expr OP_BOR bit_expr %prec OP_BOR {
            AbSyn.Expr.Binary ("|",$1,$3)
            }
        | bit_expr OP_BAND bit_expr %prec OP_BAND {
           AbSyn.Expr.Binary ("&",$1,$3)
            }
        | bit_expr OP_SHIFT_LEFT bit_expr %prec OP_SHIFT_LEFT {
           AbSyn.Expr.Binary ("<<",$1,$3)
            }
        | bit_expr OP_SHIFT_RIGHT bit_expr %prec OP_SHIFT_RIGHT {
           AbSyn.Expr.Binary (">>",$1,$3)
            }
        | bit_expr OP_PLUS bit_expr %prec OP_PLUS {
            AbSyn.Expr.Binary ("+",$1,$3)
            }
        | bit_expr OP_MINUS bit_expr %prec OP_MINUS {
            AbSyn.Expr.Binary ("-",$1,$3)
            }
        //| bit_expr OP_PLUS INTERVAL_SYM expr interval %prec OP_PLUS {}
        //| bit_expr OP_MINUS INTERVAL_SYM expr interval %prec OP_MINUS {}
        | bit_expr OP_TIMES bit_expr %prec OP_TIMES {
            AbSyn.Expr.Binary ("*",$1,$3)
            }
        | bit_expr OP_DIV bit_expr %prec OP_DIV {
            AbSyn.Expr.Binary ("/",$1,$3)
            }
        | bit_expr OP_PERC bit_expr %prec OP_PERC {
            AbSyn.Expr.Binary ("%",$1,$3)
            }
        | bit_expr OP_DIV_TXT bit_expr %prec OP_DIV_TXT {
            AbSyn.Expr.Binary ("/",$1,$3)
            }
        | bit_expr OP_MOD_TXT bit_expr %prec OP_MOD {
            AbSyn.Expr.Binary ("%",$1,$3)
            }
        | bit_expr OP_UP bit_expr {
            AbSyn.Expr.Binary ("^",$1,$3)
            }
        | simple_expr {
            $1
            }
        ;
op_or:
          OP_OR         {}
       |  OP_OR_TXT     {}
       ;

op_and:
          OP_AND        {}
       |  OP_AND_TXT    {}
       ;

op_not:
          OP_BANG       {}
        | OP_NOT_TXT    {}
        ;

comp_op:
          OP_EQ     { "eq" }
        | OP_EQ2    { "eq" }
        | OP_GEQ    { "geq" }
        | OP_GT     { "gt" }
        | OP_LEQ    { "leq" }
        | OP_LT     { "lt" }
        | OP_NEQ    { "neq" }
        ;

all_or_any:
          KEY_ALL   { "" }
        | KEY_ANY   { "" }
        ;

simple_expr:
          simple_ident                  { $1 }
        //| function_call_keyword     {}
        //| function_call_nonkeyword  {}
        //| function_call_generic     {}
        //| function_call_conflict    {}
        //| simple_expr KEY_COLLATE ident_or_text %prec OP_NEG {}
        | literal                       { $1 }
        //| param_marker { $$= $1; }
        //| variable
        //| set_function_specification
        //| window_func_call
        //| simple_expr OP_OR simple_expr {}
        | OP_PLUS simple_expr %prec OP_NEG {
            AbSyn.Expr.Unary ("+",$2)
            }
        | OP_MINUS simple_expr %prec OP_NEG {
            AbSyn.Expr.Unary ("-",$2)
            }
        | OP_TILDE simple_expr %prec OP_NEG {
            AbSyn.Expr.Unary ("~",$2)
            }
        | OP_BANG simple_expr %prec OP_NEG {
            AbSyn.Expr.Unary ("!",$2)
            }
        //| row_subquery {}
        //| PAR_LPAR expr PAR_RPAR { $$= $2; }
        //| PAR_LPAR expr DELIM_COMMA expr_list PAR_RPAR {}
        //| ROW_SYM PAR_LPAR expr DELIM_COMMA expr_list PAR_RPAR {}
        //| OP_EXISTS table_subquery {}
        //| PAR_LBRACE ident expr PAR_RBRACE {}
        //| KEY_MATCH ident_list_arg KEY_AGAINST PAR_LPAR bit_expr fulltext_options PAR_RPAR {}
        //| BINARY_SYM simple_expr %prec OP_NEG {}
        //| CAST_SYM PAR_LPAR expr KEY_AS cast_type PAR_RPAR {}
        //| CASE_SYM opt_expr when_list opt_else END {}
        //| CONVERT_SYM '(' expr ',' cast_type ')' {}
        //| CONVERT_SYM '(' expr USING charset_name ')' {}
        //| DEFAULT_SYM '(' simple_ident ')' {}
        //| VALUES '(' simple_ident_nospvar ')' {}
        //| INTERVAL_SYM expr interval '+' expr %prec INTERVAL_SYM {}
        //| simple_ident JSON_SEPARATOR_SYM TEXT_STRING_literal {}
        //| simple_ident JSON_UNQUOTED_SEPARATOR_SYM TEXT_STRING_literal {}
        ;
literal:
          text_literal          { $1 }
        | num_literal           { $1 }
        //| temporal_literal
        | VAL_NULL {
            AbSyn.Expr.NodeTyped ("keyword","null")
            }
        | VAL_FALSE     {
            AbSyn.Expr.NodeTyped ("keyword","false")
            }
        | VAL_TRUE {
            AbSyn.Expr.NodeTyped ("keyword","true")
            }
        | VAL_HEX {
            AbSyn.Expr.Temp
            }
        | VAL_BIN {
            AbSyn.Expr.Temp
            }
        //| UNDERSCORE_CHARSET HEX_NUM {}
        //| UNDERSCORE_CHARSET BIN_NUM {}
        ;

text_literal:
          VAL_STRING {
            AbSyn.Expr.NodeTyped ("string",$1)
            }
        //| NCHAR_STRING {}
        //| UNDERSCORE_CHARSET TEXT_STRING {}
        //| text_literal TEXT_STRING_literal {}
        ;
/*
Different kind of text-strings that abides some format
*/
text_string_sys:
          VAL_STRING {
            //check format!
            $1
            }
        ;
num_literal:
          VAL_NUM           { AbSyn.Expr.NodeTyped ("num",$1) }
        //| LONG_NUM {}
        //| ULONGLONG_NUM {}
        //| DECIMAL_NUM {}
        //| FLOAT_NUM {}
        ;
/*
        Identifiers
*/
ident:
    VAL_ID {
        AbSyn.Expr.NodeTyped ("id",$1)
        }
simple_ident:
          ident             { $1 }
        | simple_ident_q    { $1 }
        ;
simple_ident_nospvar:
          ident             { AbSyn.Expr.Temp }
        | simple_ident_q    { AbSyn.Expr.Temp }
        ;
simple_ident_q:
          ident OP_DOT ident {
            AbSyn.Expr.ExprList [$1;$3]
            }
        | ident OP_DOT ident OP_DOT ident {
            AbSyn.Expr.ExprList [$1;$3;$5]
            }
        ;
table_ident:
          ident                 { AbSyn.Expr.ExprList [$1] }
        | ident OP_DOT ident    { AbSyn.Expr.ExprList [$1;$3] }
        ;
table_ident_opt_wild:
          ident opt_wild                { AbSyn.Expr.ExprList ([$1] @ $2) }
        | ident OP_DOT ident opt_wild   { AbSyn.Expr.ExprList ([$1;$3] @ $4)}
        ;
opt_wild:
          /* empty */       { [] }
        | OP_DOT OP_TIMES   { [AbSyn.Expr.NodeTyped ("id","*")] }
        ;
table_wild:
          ident OP_DOT OP_TIMES {
            AbSyn.Expr.ExprList [$1;AbSyn.Expr.NodeTyped ("id","*")]
            }
        | ident OP_DOT ident OP_DOT OP_TIMES {
            AbSyn.Expr.ExprList [$1;$3;AbSyn.Expr.NodeTyped ("id","*")]
            }
        ;
%%      
