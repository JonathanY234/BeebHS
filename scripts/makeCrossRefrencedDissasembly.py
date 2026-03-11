from collections import defaultdict

def instrSize(addressingMode: str) -> int:
    match addressingMode:
        case "immediate": return 2
        case "zeropage": return 2
        case "zeropageXRMW": return 2
        case "zeropageX": return 2
        case "zeropageY": return 2
        case "absolute": return 3
        case "absoluteX": return 3
        case "absoluteXRMW": return 3
        case "absoluteY": return 3
        case "absoluteYRMW": return 3
        case "indirectX": return 2
        case "indirectY": return 2
        case "indirectYRMW": return 2
        case "indirect": return 3
        case "implied": return 1
        case "relative": return 2
        case "useAcc": return 1
        case "brk": return 1
        case _:
            print(f"error {addressingMode} not recognised")

def posOfNextInstr(currentIdx: int) -> int:
    currentInstr = rom_bytes[currentIdx]
    try:
        size = opcode_to_size[currentInstr]
    except:
        print(f"Its not an known instruction {hex(currentInstr)}")
        size = 1
    
    return currentIdx + size

def posOfNextInstrLinearSearch(currentInstr: int, opcodes: list[int]) -> int:

    nextInstr = currentInstr + 1
    if currentInstr == 7130 or currentInstr == 0x2267:
        print (f"currentInstr {currentInstr}, nextInstr {nextInstr}")
    while (rom_bytes[nextInstr] not in opcodes):
        nextInstr += 1
    # remaining = rom_bytes[currentInstr:]
    # idx = remaining.index(opcode)

    print(f"Gap: {hex(nextInstr)} + {nextInstr - currentInstr}")

    return nextInstr

def getOpcodes(mnemonic: str) -> list[int]:
    opcodes = mne_to_opcodes[mnemonic]
    if opcode is None:
        print("Unknown mnemonic:", mnemonic)
    else:
        return opcodes

# Step 1: build the instruction opcode table

chapterSizes = [0, 0, 0, 1216, 1629, 475, 872, 1890, 382, 732, 681, 479, 362, 933, 1360, 983, 1107, 801, 575, 280, 768, 167, 89]

def romStartFrom(chapter: int) -> int:
    sum = 0
    for i in range(chapter-1):
        sum += chapterSizes[i]
    return sum

print(f"sum of chapters {sum(chapterSizes)}")

filepath = "app/CPU6502.hs"
opcodeTableStart = 180 -1
opcodeTableEnd = 331


opcode_to_size = {}
mne_to_opcodes = defaultdict(list)

hSourceFile = open(filepath, "r")

# seek to the lines we want
for i in range(opcodeTableStart):
    hSourceFile.readline()

for i in range(opcodeTableEnd - opcodeTableStart):
    line = hSourceFile.readline()
    cleanedLine = line[8:-1]
    words = cleanedLine.split(" ")

    mnemonic = words[4][:3].upper()

    if mnemonic == 'UND':
        mnemonic = 'BRK'

    addressingMode = words[3]
    opcode = int(words[1][2:], 16)
    
    mne_to_opcodes[mnemonic].append(opcode)
    opcode_to_size[opcode] = instrSize(addressingMode)

hSourceFile.close()

# Step 2: match dissasambly and and assembled ROM

import re
mnemonic_re = re.compile(r'\s([A-Z]{3})\s')

rom_path = "roms/os12.rom"
disasm_path = "scripts/os120_acme.txt"
output_path = "scripts/os120crossrefrenced.txt"

OS_START_IDX = 0xC000

disasm_file = open(disasm_path, "r")
output_file = open(output_path, "w")

with open(rom_path, "rb") as f:
    rom_bytes = bytearray(f.read())
#print(len(rom_bytes))

with open(disasm_path) as f:
    disasm_lines = f.readlines()

def writeStartOrEnd(start: bool):
    if start:
        firstLine = 0
        lastLine = 81
    else:
        firstLine = 20577
        lastLine = 20917

    for i in range(firstLine, lastLine):
        line = disasm_lines[i]
        output_file.write(line)


def annotateChapter(chapterNo: int):
    rom_idx = romStartFrom(chapterNo)
    
    print(f"Chapter {chapterNo}_____________________________")

    skipChapter = False

    # rom_idx is set based on information from the dissasembly. But its not always accurate so correction here
    if chapterNo == 4: #account for the table of chars
        rom_idx += 752
    elif chapterNo == 10:
        rom_idx += 141
    elif chapterNo == 18:
        rom_idx -= 504
    elif chapterNo == 19:
        rom_idx -= 198
    elif chapterNo == 20:
        rom_idx += 28
    elif chapterNo == 21:
        rom_idx += 323
    elif chapterNo == 22: # this one is just credits
        skipChapter = True
    elif chapterNo == 23:
        rom_idx += 436

    #seek to the start of the chapter
    
    chapter_regex = re.compile(f"; Chapter {chapterNo}: ")
    next_chapter_regex = re.compile(f"; Chapter {chapterNo+1}: ")
    for text_idx, line in enumerate(disasm_lines):
        
        match = chapter_regex.search(line)
        if match:
            #print(f"Found Chapter start {line}")
            chapter_start = text_idx

        match = next_chapter_regex.search(line)
        if match:
            #print(f"Found Chapter end {line}")
            chapter_end = text_idx
            break

    lines_in_chapter = disasm_lines[chapter_start:chapter_end]

    for line in lines_in_chapter:

        code_part = line.split(";", 1)[0] # dont allow matches in comments
        match = mnemonic_re.search(code_part)
        if not match or skipChapter:
            output_file.write(line)
            continue # doesnt contain code 

        mnemonic = match.group(1)
        
        possible_opcodes = getOpcodes(mnemonic)# Need to handle multiple possible opcodes for one mnemonic for each addressing mode

        # if chapterNo == 14:
        #     print(f"rom_idx is {hex(rom_idx)}")

        if rom_bytes[rom_idx] in possible_opcodes:
            pass
        else:
            rom_idx = posOfNextInstrLinearSearch(rom_idx, possible_opcodes) # will find a match but might not be correct one


        newline = line.rstrip("\n") + "  $^$ " + hex(OS_START_IDX + rom_idx) + "\n"
        output_file.write(newline)

        rom_idx = posOfNextInstr(rom_idx) # advance to the next instruction (hopefully)

writeStartOrEnd(True)

for i in range(1, 25):
    annotateChapter(i)

writeStartOrEnd(False)


output_file.close()