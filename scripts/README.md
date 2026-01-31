This folder contains two scripts created to assist in developing the emulator. But they are not used by the finished emulator itself.

## makeMnemonicTable.py

Reads the opcodeTable from the haskell source file and generates a mnemonic to copy paste into the emulator debugger code and enables the debugger to map opcodes to mnemonics. By generating the second table from the first can ensure that both remain synced and work is not manually duplicated as more instructions are added.

## makeCrossRefrencedDissasembly.py

Reads the opcodeTable from the Haskell source, the assembled os12.rom, and the os12 ROM disassembly by Tony Nelson. It then annotates each instruction in the disassembly with its corresponding memory location. This makes debugging easier, as instructions in memory can be directly referenced in the disassembly to understand what they do.

Currently Chapters 18 and 19 are skipped because they cause errors and fixing them is not a priority