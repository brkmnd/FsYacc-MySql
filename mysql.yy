%{
//AbSyn
module AbSyn =
    type Q_Select =
        | SelectNull
        | SelectOptions of Option<string>
        | SelectItems of (string * string) list
    type Qs =
        | Select of Q_Select list
%}

//delimiters
%token DELIM_SCOLON DELIM_COMMA END_OF_INPUT
//pars
%token PAR_LPAR PAR_RPAR PAR_LRBACE PAR_RBRACE
//operators
%token OP_PLUS OP_MINUS OP_DIV OP_TIMES OP_PERC OP_DOT
%token OP_NOT OP_OR OP_XOR OP_AND OP_TILDE
//keywords
%token KEY_SELECT
%token KEY_AS
%token KEY_IS

//values
%token<string>VAL_ID
%token<string>VAL_NUM
%token<string>VAL_STRING
%token VAL_NULL
%token VAL_TRUE
%token VAL_FALSE
%token VAL_UNKNOWN

%start start_entry
%type<AbSyn.Qs list> start_entry

%%
start_entry:
          sql_statement { $1 }
        /*
        | GRAMMAR_SELECTOR_EXPR bit_expr END_OF_INPUT {
            ITEMIZE($2, &$2);
            static_cast<Expression_parser_state *>(YYP)->result= $2;
            }
        | GRAMMAR_SELECTOR_PART partition_clause END_OF_INPUT {
              //We enter here when translating partition info string into
              //partition_info data structure.
            CONTEXTUALIZE($2);
            static_cast<Partition_expr_parser_state *>(YYP)->result=
              &$2->part_info;
            }
        | GRAMMAR_SELECTOR_GCOL IDENT_sys LPAR expr RPAR END_OF_INPUT {
            
              //We enter here when translating generated column info string into
              //partition_info data structure.
            

            // Check gcol expression for the "PARSE_GCOL_EXPR" prefix:
            if (!is_identifier($2, "PARSE_GCOL_EXPR"))
              MYSQL_YYABORT;

            auto gcol_info= NEW_PTN Value_generator;
            if (gcol_info == NULL)
              MYSQL_YYABORT; // OOM
            ITEMIZE($4, &$4);
            gcol_info->expr_item= $4;
            static_cast<Gcol_expr_parser_state *>(YYP)->result= gcol_info;
            }
        | GRAMMAR_SELECTOR_CTE table_subquery END_OF_INPUT {
            static_cast<Common_table_expr_parser_state *>(YYP)->result= $2;
            }
        */
        ;

sql_statement:
          END_OF_INPUT {
            //empty statement
            []
            //THD *thd= YYTHD;
            //if (!thd->is_bootstrap_system_thread() &&
            //    !thd->m_parser_state->has_comment())
            //{
            //  my_error(ER_EMPTY_QUERY, MYF(0));
            //  MYSQL_YYABORT;
            //}
            //thd->lex->sql_command= SQLCOM_EMPTY_QUERY;
            //YYLIP->found_semicolon= NULL;
            }
        | simple_statement_or_begin opt_end_of_input DELIM_SCOLON {
            [$1]
            //we found a wellformed query...
            }
        | simple_statement_or_begin END_OF_INPUT {
            [$1]
            //single query, not terminated.
            }
        ;

opt_end_of_input:
        /* empty */     {}
        | END_OF_INPUT  {}
        ;

simple_statement_or_begin:
          simple_statement { $1
            //*parse_tree= $1;
            }
        //| begin_stmt
        ;

simple_statement:
        //comment out those statements that are not going to be used in the
        //parse-tree generator
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
       | select_stmt           {
        $1
       }
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
select_stmt:
          query_expression {
            $1
            //$$= NEW_PTN PT_select_stmt($1);
            }
        /*
        | query_expression_parens {
            if ($1 == NULL)
              MYSQL_YYABORT; // OOM
            $$= NEW_PTN PT_select_stmt($1);
            }
        */
        //| select_stmt_with_into {}
        ;

query_expression:
          query_expression_body
          //opt_order_clause
          //opt_limit_clause
          //opt_locking_clause_list
          {
          $1
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
            }
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

query_expression_body:
          query_primary {
            $1
            //$$= NEW_PTN PT_query_expression_body_primary($1);
            }
        /*
        | query_expression_body UNION_SYM union_option query_primary {
            $$= NEW_PTN PT_union(NEW_PTN PT_query_expression($1), @1, $3, $4);
            }
        | query_expression_parens UNION_SYM union_option query_primary {
            if ($1 == NULL)
              MYSQL_YYABORT; // OOM

            $1->set_parentheses();

            $$= NEW_PTN PT_union($1, @1, $3, $4);
            }
        | query_expression_body UNION_SYM union_option query_expression_parens {
            if ($4 == NULL)
              MYSQL_YYABORT; // OOM

            if ($4->is_union())
              YYTHD->syntax_error_at(@4);

            auto lhs_qe= NEW_PTN PT_query_expression($1);
            PT_nested_query_expression *nested_qe=
              NEW_PTN PT_nested_query_expression($4);

            $$= NEW_PTN PT_union(lhs_qe, @1, $3, nested_qe);
            }
        | query_expression_parens UNION_SYM union_option query_expression_parens {
            if ($1 == NULL || $4 == NULL)
              MYSQL_YYABORT; // OOM

            if ($4->is_union())
              YYTHD->syntax_error_at(@4);

            $1->set_parentheses();

            PT_nested_query_expression *nested_qe=
              NEW_PTN PT_nested_query_expression($4);
            $$= NEW_PTN PT_union($1, @1, $3, nested_qe);
            }
            */
        ;

query_primary:
          query_specification {
            $1
            // Bison doesn't get polymorphism.
           // $$= $1;
            }
        ;

query_specification:
          KEY_SELECT
          select_options
          select_item_list
          //into_clause
          //opt_from_clause
          //opt_where_clause
          //opt_group_clause
          //opt_having_clause
          //opt_window_clause
          {
          AbSyn.Qs.Select [
            AbSyn.Q_Select.SelectOptions $2  //Select options
            AbSyn.Q_Select.SelectItems $3
            ]
          (*
            $$= NEW_PTN PT_query_specification(
                                      $1,  // SELECT_SYM
                                      $2,  // select_options
                                      $3,  // select_item_list
                                      $4,  // into_clause
                                      $5,  // from
                                      $6,  // where
                                      $7,  // group
                                      $8,  // having
                                      $9); // windows
            *)
            }
            /*
        | SELECT
          //select_options
          //select_item_list
          //opt_from_clause
          //opt_where_clause
          //opt_group_clause
          //opt_having_clause
          //opt_window_clause
          {
            $$= NEW_PTN PT_query_specification(
                                      $1,  // SELECT_SYM
                                      $2,  // select_options
                                      $3,  // select_item_list
                                      NULL,// no INTO clause
                                      $4,  // from
                                      $5,  // where
                                      $6,  // group
                                      $7,  // having
                                      $8); // windows
            }
            */
        ;
select_options:
          /* empty*/ {
            None
            }
        //| select_option_list
        ;
/*
select_option_list:
          select_option_list select_option
          {
            if ($$.merge($1, $2))
              MYSQL_YYABORT;
          }
        | select_option
        ;
select_option:
          query_spec_option
          {
            $$.query_spec_options= $1;
          }
        | SQL_NO_CACHE_SYM
          {
            push_deprecated_warn_no_replacement(YYTHD, "SQL_NO_CACHE");
            //Ignored since MySQL 8.0.
            $$.query_spec_options= 0;
          }
        ;
*/
select_item_list:
          select_item_list DELIM_COMMA select_item {
            //if ($1 == NULL || $1->push_back($3))
            //  MYSQL_YYABORT;
            //$$= $1;
            $1 @ [$3]
            }
        | select_item {
            //$$= NEW_PTN PT_select_item_list;
            //if ($$ == NULL || $$->push_back($1))
            //  MYSQL_YYABORT;
            [$1]
            }
        | OP_TIMES {
            //Item *item= NEW_PTN Item_field(@$, NULL, NULL, "*");
            //$$= NEW_PTN PT_select_item_list;
            //if ($$ == NULL || $$->push_back(item))
            //  MYSQL_YYABORT;
            [("*","")]
            }
        ;

select_item:
          table_wild {
            ($1,"")
            }
        | expr select_alias {
            //$$= NEW_PTN PTI_expr_with_alias(@$, $1, @1.cpp, $2);
            ($1,$2)
            }
        ;
select_alias:
          /* empty */   { "" }
        | KEY_AS ident      { $2 }
        | KEY_AS VAL_STRING    { $2 }
        | ident         { $1 }
        | VAL_STRING       { $1 }
        ;

/*
        Expressions
*/
expr:
          expr or expr %prec OP_OR {
            $$= flatten_associative_operator<Item_cond_or,
                                             Item_func::COND_OR_FUNC>(
                                                 YYMEM_ROOT, @$, $1, $3);
            }
        | expr OP_XOR expr %prec OP_XOR {
            /* XOR is a proprietary extension */
            $$ = NEW_PTN Item_func_xor(@$, $1, $3);
            }
        | expr and expr %prec OP_AND {
            $$= flatten_associative_operator<Item_cond_and,
                                             Item_func::COND_AND_FUNC>(
                                                 YYMEM_ROOT, @$, $1, $3);
            }
        | OP_NOT expr %prec OP_NOT {
            $$= NEW_PTN PTI_negate_expression(@$, $2);
            }
        | bool_pri KEY_IS VAL_TRUE %prec KEY_IS {
            $$= NEW_PTN Item_func_istrue(@$, $1);
            }
        | bool_pri KEY_IS not VAL_TRUE %prec KEY_IS {
            $$= NEW_PTN Item_func_isnottrue(@$, $1);
            }
        | bool_pri KEY_IS VAL_FALSE %prec KEY_IS {
            $$= NEW_PTN Item_func_isfalse(@$, $1);
            }
        | bool_pri KEY_IS not VAL_FALSE %prec KEY_IS {
            $$= NEW_PTN Item_func_isnotfalse(@$, $1);
            }
        | bool_pri KEY_IS VAL_UNKNOWN %prec KEY_IS {
            $$= NEW_PTN Item_func_isnull(@$, $1);
            }
        | bool_pri KEY_IS not VAL_UNKNOWN %prec KEY_IS {
            $$= NEW_PTN Item_func_isnotnull(@$, $1);
            }
        | bool_pri {}
        ;
bool_pri:
          bool_pri KEY_IS VAL_NULL %prec KEY_IS {
            $$= NEW_PTN Item_func_isnull(@$, $1);
            }
        | bool_pri KEY_IS not VAL_NULL %prec KEY_IS {
            $$= NEW_PTN Item_func_isnotnull(@$, $1);
          }
        | bool_pri comp_op predicate {
            $$= NEW_PTN PTI_comp_op(@$, $1, $2, $3);
            }
        | bool_pri comp_op all_or_any table_subquery %prec OP_EQ {
            if ($2 == &comp_equal_creator)
              /*
                We throw this manual parse error rather than split the rule
                comp_op into a null-safe and a non null-safe rule, since doing
                so would add a shift/reduce conflict. It's actually this rule
                and the ones referencing it that cause all the conflicts, but
                we still don't want the count to go up.
              */
              YYTHD->syntax_error_at(@2);
            $$= NEW_PTN PTI_comp_op_all(@$, $1, $2, $3, $4);
            }
        | predicate {}
        ;
predicate:
          bit_expr KEY_IN table_subquery {
            $$= NEW_PTN Item_in_subselect(@$, $1, $3);
            }
        | bit_expr not OP_IN table_subquery {
            Item *item= NEW_PTN Item_in_subselect(@$, $1, $4);
            $$= NEW_PTN PTI_negate_expression(@$, item);
            }
        | bit_expr OP_IN PAR_LPAR expr PAR_RPAR {
            $$= NEW_PTN PTI_handle_sql2003_note184_exception(@$, $1, true, $4);
            }
        | bit_expr OP_IN PAR_LPAR expr DELIM_COMMA expr_list PAR_RPAR {
            if ($6 == NULL || $6->push_front($4) || $6->push_front($1))
              MYSQL_YYABORT;

            $$= NEW_PTN Item_func_in(@$, $6, false);
            }
        | bit_expr not OP_IN PAR_LPAR expr PAR_RPAR {
            $$= NEW_PTN PTI_handle_sql2003_note184_exception(@$, $1, false, $5);
            }
        | bit_expr not OP_IN PAR_LPAR expr DELIM_COMMA expr_list PAR_RPAR {
            if ($7 == NULL || $7->push_front($5) || $7->value.push_front($1))
              MYSQL_YYABORT;

            $$= NEW_PTN Item_func_in(@$, $7, true);
            }
        | bit_expr OP_BETWEEN bit_expr OP_AND predicate {
            $$= NEW_PTN Item_func_between(@$, $1, $3, $5, false);
            }
        | bit_expr not OP_BETWEEN bit_expr OP_AND predicate {
            $$= NEW_PTN Item_func_between(@$, $1, $4, $6, true);
          }
        | bit_expr OP_SOUNDS OP_LIKE bit_expr {
            Item *item1= NEW_PTN Item_func_soundex(@$, $1);
            Item *item4= NEW_PTN Item_func_soundex(@$, $4);
            if ((item1 == NULL) || (item4 == NULL))
              MYSQL_YYABORT;
            $$= NEW_PTN Item_func_eq(@$, item1, item4);
            }
        | bit_expr OP_LIKE simple_expr opt_escape {
            $$= NEW_PTN Item_func_like(@$, $1, $3, $4);
            }
        | bit_expr not OP_LIKE simple_expr opt_escape {
            Item *item= NEW_PTN Item_func_like(@$, $1, $4, $5);
            if (item == NULL)
              MYSQL_YYABORT;
            $$= NEW_PTN Item_func_not(@$, item);
            }
        | bit_expr KEY_REGEXP bit_expr {
            auto args= NEW_PTN PT_item_list;
            args->push_back($1);
            args->push_back($3);

            $$= NEW_PTN Item_func_regexp_like(@1, args);
            }
        | bit_expr not KEY_REGEXP bit_expr {
            auto args= NEW_PTN PT_item_list;
            args->push_back($1);
            args->push_back($4);
            Item *item= NEW_PTN Item_func_regexp_like(@$, args);
            $$= NEW_PTN PTI_negate_expression(@$, item);
            }
        | bit_expr {}
        ;

bit_expr:
          bit_expr OP_BOR bit_expr %prec OP_BOR {
            $$= NEW_PTN Item_func_bit_or(@$, $1, $3);
            }
        | bit_expr OP_BAND bit_expr %prec OP_BAND {
            $$= NEW_PTN Item_func_bit_and(@$, $1, $3);
            }
        | bit_expr OP_SHIFT_LEFT bit_expr %prec OP_SHIFT_LEFT {
            $$= NEW_PTN Item_func_shift_left(@$, $1, $3);
            }
        | bit_expr OP_SHIFT_RIGHT bit_expr %prec OP_SHIFT_RIGHT {
            $$= NEW_PTN Item_func_shift_right(@$, $1, $3);
            }
        | bit_expr OP_PLUS bit_expr %prec OP_PLUS {
            $$= NEW_PTN Item_func_plus(@$, $1, $3);
            }
        | bit_expr OP_MINUS bit_expr %prec OP_MINUS {
            $$= NEW_PTN Item_func_minus(@$, $1, $3);
            }
        | bit_expr OP_PLUS INTERVAL_SYM expr interval %prec OP_PLUS {
            $$= NEW_PTN Item_date_add_interval(@$, $1, $4, $5, 0);
            }
        | bit_expr OP_MINUS INTERVAL_SYM expr interval %prec OP_MINUS {
            $$= NEW_PTN Item_date_add_interval(@$, $1, $4, $5, 1);
            }
        | bit_expr OP_TIMES bit_expr %prec OP_TIMES {
            $$= NEW_PTN Item_func_mul(@$, $1, $3);
            }
        | bit_expr OP_DIV bit_expr %prec OP_DIV {
            $$= NEW_PTN Item_func_div(@$, $1,$3);
            }
        | bit_expr OP_PERC bit_expr %prec OP_PERC {
            $$= NEW_PTN Item_func_mod(@$, $1,$3);
            }
        | bit_expr OP_DIV_SYM bit_expr %prec OP_DIV_SYM {
            $$= NEW_PTN Item_func_int_div(@$, $1,$3);
            }
        | bit_expr OP_MOD bit_expr %prec OP_MOD {
            $$= NEW_PTN Item_func_mod(@$, $1, $3);
            }
        | bit_expr OP_UP bit_expr {
            $$= NEW_PTN Item_func_bit_xor(@$, $1, $3);
            }
        | simple_expr {}
        ;
or:
          OP_OR  {}
       |  OP_OR2 {}
       ;

and:
          OP_AND   {}
       |  OP_AND2  {}
       ;

not:
          OP_NOT    {}
        | OP_NOT2   {}
        ;

not2:
          OP_BANG   {}
        | OP_NOT2   {}
        ;

comp_op:
          OP_EQ     { $$ = &comp_eq_creator; }
        | OP_EQ2    { $$ = &comp_equal_creator; }
        | OP_GEQ    { $$ = &comp_ge_creator; }
        | OP_GT     { $$ = &comp_gt_creator; }
        | OP_LEQ    { $$ = &comp_le_creator; }
        | OP_LT     { $$ = &comp_lt_creator; }
        | OP_NE     { $$ = &comp_ne_creator; }
        ;

all_or_any:
          KEY_ALL   { $$ = 1; }
        | KEY_ANY   { $$ = 0; }
        ;

simple_expr:
          simple_ident              {}
        //| function_call_keyword     {}
        //| function_call_nonkeyword  {}
        //| function_call_generic     {}
        //| function_call_conflict    {}
        //| simple_expr KEY_COLLATE ident_or_text %prec OP_NEG {}
        | literal {}
        //| param_marker { $$= $1; }
        //| variable
        //| set_function_specification
        //| window_func_call
        //| simple_expr OP_OR simple_expr {}
        | OP_PLUS simple_expr %prec OP_NEG {}
        | OP_MINUS simple_expr %prec OP_NEG {}
        | OP_TILDE simple_expr %prec OP_NEG {}
        | not2 simple_expr %prec OP_NEG {}
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
          text_literal  { $$= $1; }
        | VAL_NUM       { $$= $1; }
        //| temporal_literal
        | VAL_NULL      {}
        | VAL_FALSE     {
            //$$= NEW_PTN Item_int(@$, NAME_STRING("FALSE"), 0, 1);
          }
        | VAL_TRUE {
            $$= NEW_PTN Item_int(@$, NAME_STRING("TRUE"), 1, 1);
            }
        | VAL_HEX {
            //$$= NEW_PTN Item_hex_string(@$, $1);
            }
        | VAL_BIN {
            //$$= NEW_PTN Item_bin_string(@$, $1);
            }
        //| UNDERSCORE_CHARSET HEX_NUM {}
        //| UNDERSCORE_CHARSET BIN_NUM {}
        ;

NUM_literal:
          VAL_NUM {
            $$= NEW_PTN Item_int(@$, $1);
            }
        //| LONG_NUM {}
        //| ULONGLONG_NUM {}
        //| DECIMAL_NUM {}
        //| FLOAT_NUM {}
        ;
/*
        Identifiers
*/
ident:
    VAL_ID { $1 }
table_wild:
          ident OP_DOT OP_TIMES {
            $1
            //$$= NEW_PTN PTI_table_wild(@$, NULL, $1.str);
            }
        | ident OP_DOT ident OP_DOT OP_TIMES {
            $1
            //$$= NEW_PTN PTI_table_wild(@$, $1.str, $3.str);
            }
        ;
%%      
