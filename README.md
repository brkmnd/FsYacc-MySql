This is a version of the MySql grammar/parser originally written in Yacc/Bison, but rewritten to fit FsYacc.

Building:
I have had problems using the -r compiler flag along --standalone. I think the problem occurs because of different FSharp.Core.dll versions within my computer and the core file shipped with FsLexYacc. The best solution I found is to add the lexer and parser from the runtime of the FsLexYacc repo. These have been placed in lib/.
Build using ./build.sh - it might need to be chmod'ed. The target is a standalone MbSqlDriver.dll. Since it is standalone it can be run from ex. a C# project.
