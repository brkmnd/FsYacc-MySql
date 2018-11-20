mono ../FsLexYacc.7.0.6/build/fsyacc.exe --module "MbSqlParser" parser.yy

#dlls
core="../FSharp.Core.3.1.2.5/lib/portable-net45+netcore45/FSharp.Core.dll" 
syst="/usr/lib/mono/xbuild-frameworks/.NETPortable/v4.5/System.dll"
yacc="../FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
srun="/usr/lib/mono/xbuild-frameworks/.NETPortable/v4.5/System.Runtime.dll"
coll="/usr/lib/mono/xbuild-frameworks/.NETPortable/v4.5/System.Collections.dll"
sysio="/usr/lib/mono/xbuild-frameworks/.NETPortable/v4.5/System.IO.dll"
mscore="/usr/lib/mono/xbuild-frameworks/.NETPortable/v4.5/mscorlib.dll"
netstd="/usr/lib/mono/xbuild/Microsoft/Microsoft.NET.Build.Extensions/net461/lib/netstandard.dll"
glob="/usr/lib/mono/xbuild-frameworks/.NETPortable/v4.5/System.Globalization.dll"
regex="/usr/lib/mono/xbuild-frameworks/.NETPortable/v4.5/System.Text.RegularExpressions.dll"
rs=" -r $core -r $syst -r $mscore -r $netstd -r $srun -r $coll -r $sysio -r $glob -r $regex -r $yacc "

fsharpc $rs --target:library absyn.fs traverse.fs parser.fs lexer.fs MbSqlDriver.fs --noframework
#fsharpc -r $yacc --target:library absyn.fs traverse.fs parser.fs lexer.fs MbSqlDriver.fs --standalone
