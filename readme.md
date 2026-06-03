# BeebHS: An Emulator for the BBC Micro

## Design overview
The core of the system is a MOS 6502CPU interpreter. It mutates an IOVector representing the BBC Micro’s RAM within the IO monad. Other subsystems such as System Via (for keyboard input) and Mode 7 Rendering (for display), operate through memory mapped I/O, where device behaviour is triggered by reads and writes to specific address ranges. The disk filing system (DFS) implementation differs from the above approach. Instead of fully emulating a floppy disk controller, disk operations are intercepted and routed to haskell functions which read from disk image files.

## Compatibility

The emulator is capable of booting the BBC Micro OS and running BBC BASIC, as well as some external disk-based programs, including:

SnapperV2.ssd, SpaceInvadersArcadeAction.ssd, HampsteadSST.ssd, SlotMachine.ssd, MapQuizMode7.ssd

## Requirements
- GHC
- Cabal
- SDL2 libraries

The BBC Micro OS ROM and BASIC ROM are required but not included due to licensing restrictions.
Place ROMs in:

roms/  
├── basic2.rom  
└── os12.rom  

## Build and Run
`cabal run BeebHS`

## Launch Options

- `-cpuTest` to run cpu unit tests. Tests cases are not included, available here https://github.com/SingleStepTests/65x02

- `-debug` to start the 6502 cpu debugger. Type `help` to see available commands
