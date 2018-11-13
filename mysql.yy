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
%token<string>VAL_LITERAL
%token VAL_NULL
%token VAL_TRUE
%token VAL_FALSE

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
          //hertil
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
            //hertil
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
        | KEY_AS VAL_LITERAL    { $2 }
        | ident         { $1 }
        | VAL_LITERAL       { $1 }
        ;

/*
        Expressions
*/
//hertil - imp alle exprs fra oprindel grammar
expr:
      VAL_ID { $1 }
    | VAL_NUM OP_TIMES VAL_NUM {
        $1
        }


/*
        Identities
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
