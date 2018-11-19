parser_fs = open("parser.fs","r")
namedparser_fs = open("namedparser.fs","w")


lines = parser_fs.readlines()
indent = "  "
def writeline(line):
    namedparser_fs.write(line)
for i in range(0,len(lines)):
    line = lines[i]
    if i == 0:
        #add topline comment
        writeline(line)
        writeline("namespace MbSql\n")
    elif i == 1:
        writeline(line[:-1] + " =\n")
    elif line[0] != "#":
    #else:
        writeline(indent + line.replace("parser.fs","namedparser.fs"))
print("naming done")
