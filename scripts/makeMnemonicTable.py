filepath = "app/CPU6502.hs"
opcodeTableStart = 143
opcodeTableEnd = 293

file = open(filepath, "r")

# seek to the lines we want
for i in range(opcodeTableStart):
    file.readline()

print("    [ --Generated from opcode table by makeMnemonicTable.py")
for i in range(opcodeTableEnd - opcodeTableStart):
    line = file.readline()
    cleanedLine = line[8:-1]
    words = cleanedLine.split(" ")

    correctedMnemonic = words[4][:3].upper()
    print(f'    ({words[1]}, "{words[3].capitalize()} {correctedMnemonic}"),', end="")
    if i % 3 == 0:
        print("")
print("    ]")
file.close()

sum = 0
for i in range(0,11):
   if i % 2 == 0:
      sum += i*i