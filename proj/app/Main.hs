module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)

import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.Word as W
import CPU.CPU
import CPU.Emulate
import CPU.Drawing
import System.Random


emptyRom = V.fromList [0, 0]

testRom = 
    V.fromList [0, 0xE0, 0, 0xE0, 0, 0xE0, 0, 0xE0, 0, 0xE0]

testRom2 =
    V.fromList [0x6A, 0xD, 0x61, 0x1, 0x62, 0x6, 0xFA, 0x29, 0xD1, 0x25,
                0x6A, 0xE, 0x61, 0x9, 0xFA, 0x29, 0xD1, 0x25,
                0x6A, 0xA, 0x61, 0x12, 0xFA, 0x29, 0xD1, 0x25,
                0x6A, 0xD, 0x61, 0x3D, 0xFA, 0x29, 0xD1, 0x25]

testRom3 = 
    V.fromList [0xF0, 0x65, 0x6A, 0xD, 0x61, 0x1, 0x62, 0x6, 0xFA, 0x29, 0xD1, 0x25,
    0x6A, 0xE, 0x61, 0x9, 0xFA, 0x29, 0xD1, 0x25]

testCPU rng =
    let cpu = defaultCPU emptyRom rng
        cpu1 = execOpcode cpu (0xA, 0, 0, 0)
        cpuu = setReg cpu1 0xA 0xF
        cpu2 = setReg cpuu 1 10
        cpu3 = setReg cpu2 2 6
        cpu4 = execOpcode cpu3 (0xF, 0xA, 0x2, 0x9)
        cpu5 = execOpcode cpu4 (0xD, 1, 2, 5)
    in cpu5


getRom :: FilePath -> IO (V.Vector W.Word8)
getRom path = do
    file <- B.readFile path
    let rom = V.fromList $ B.unpack file
    let len = V.length rom
    if len > 0xE00 || (len `mod` 2 /= 0)
        then error "slaby ten twoj rom cos"
        else return rom

main :: IO ()
main = do
    --rom <- getRom "./roms/TICTAC" -- 80 fps
    --rom <- getRom "./roms/PONG" -- 300 fps
    --rom <- getRom "./roms/BLINKY" -- 600 fps
    --rom <- getRom "./roms/CONNECT4" -- 50 fps
    --rom <- getRom "./roms/TETRIS" -- 200 fps
    --rom <- getRom "./roms/UFO" -- 300 fps
    --let rom = testRom2
    -- let mem = B.pack (V.toList $ initMem rom)
    -- B.writeFile "TEST" mem
    size <- getScreenSize
    let rng = mkStdGen 0   
    let cpu = defaultCPU rom rng
    --stepEmu size 60 cpu
    startEmu size 300 cpu



