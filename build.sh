mono ../FsLexYacc.7.0.6/build/fsyacc.exe --module "MbSqlParser" parser.yy

fsharpc --target:library lib/FsLexYacc.Runtime.Lexing.fs lib/FsLexYacc.Runtime.Parsing.fs absyn.fs traverse.fs parser.fs lexer.fs MbSqlDriver.fs --standalone
