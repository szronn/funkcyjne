module CPU.CPU where

import Data.List
import qualified Data.Vector as V
import qualified Data.Word as W (Word8, Word16)
import System.Random

data CPU = Cpu { v :: V.Vector W.Word8            
               , i :: W.Word16               
               , sound :: W.Word8     
               , delay :: W.Word8     
               , pc :: W.Word16              
               , memory :: V.Vector W.Word8        
               , stack :: V.Vector W.Word16         
               , sp :: W.Word16              
               , vram :: V.Vector Bool        
               , keyboard :: V.Vector Bool     
               , rgen :: StdGen            
               } deriving (Show)


defaultCPU :: V.Vector W.Word8 -> StdGen -> CPU  
defaultCPU rom rng = Cpu { v = V.replicate 16 0
                         , i = 0x200
                         , sound = 0
                         , delay = 0
                         , pc = 0x200
                         , memory = initMem rom
                         , stack = V.replicate 16 0
                         , sp = 0
                         , vram = emptyScreen
                         , keyboard = V.replicate 16 False
                         , rgen = rng
                        }   



initMem :: V.Vector W.Word8 -> V.Vector W.Word8
initMem rom = fontset V.++ V.replicate (0x200 - V.length fontset) 0 V.++ padRom rom

clearKeyboard :: CPU -> CPU
clearKeyboard cpu = cpu { keyboard = V.replicate 16 False }

padRom :: V.Vector W.Word8 -> V.Vector W.Word8
padRom rom
  | memLeft < 0 = error "program jest za duzy"
  | null rom = error "pusty program"
  | otherwise = rom V.++ V.replicate memLeft 0
    where memLeft = 0xE00 - V.length rom

emptyScreen :: V.Vector Bool
emptyScreen = V.replicate (32 * 64) False

fontset :: V.Vector W.Word8
fontset = V.fromList [ 0xF0, 0x90, 0x90, 0x90, 0xF0 -- 0
          , 0x20, 0x60, 0x20, 0x20, 0x70 -- 1
          , 0xF0, 0x10, 0xF0, 0x80, 0xF0 -- 2
          , 0xF0, 0x10, 0xF0, 0x10, 0xF0 -- 3
          , 0x90, 0x90, 0xF0, 0x10, 0x10 -- 4
          , 0xF0, 0x80, 0xF0, 0x10, 0xF0 -- 5
          , 0xF0, 0x80, 0xF0, 0x90, 0xF0 -- 6
          , 0xF0, 0x10, 0x20, 0x40, 0x40 -- 7
          , 0xF0, 0x90, 0xF0, 0x90, 0xF0 -- 8
          , 0xF0, 0x90, 0xF0, 0x10, 0xF0 -- 9
          , 0xF0, 0x90, 0xF0, 0x90, 0x90 -- A
          , 0xE0, 0x90, 0xE0, 0x90, 0xE0 -- B
          , 0xF0, 0x80, 0x80, 0x80, 0xF0 -- C
          , 0xE0, 0x90, 0x90, 0x90, 0xE0 -- D
          , 0xF0, 0x80, 0xF0, 0x80, 0xF0 -- E
          , 0xF0, 0x80, 0xF0, 0x80, 0x80 -- F
          ]