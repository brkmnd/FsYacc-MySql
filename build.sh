mono ../FsLexYacc.7.0.6/build/fsyacc.exe --module "MbSqlParser" parser.yy
#python add_namespace.py
fsharpc -r "/usr/lib/mono/xbuild/Microsoft/Microsoft.NET.Build.Extensions/net461/lib/System.Runtime.dll" -r "/usr/lib/mono/fsharp/FSharp.Core.dll" -r "../FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll" --target:library absyn.fs traverse.fs parser.fs lexer.fs MbSqlDriver.fs --noframework
