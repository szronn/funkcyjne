module CPU.Emulate where

import CPU.CPU as CPU
import Data.Bits
import System.Random
import System.Exit
import Data.List
import qualified Data.Word as W (Word8, Word16)
import qualified Data.Vector as V
import Control.Monad
import Numeric (showHex)
import Debug.Trace

type Opcode = (W.Word8, W.Word8, W.Word8, W.Word8)


-- procesor tyka
tick :: CPU -> CPU
tick cpu = decreseTimers $ execOpcode cpu (fetchOpcode cpu)

iter n f = foldl (.) id (replicate n f)

dekaTick = iter 10 tick
hectaTick = iter 100 tick

decreseTimers :: CPU -> CPU
decreseTimers cpu = cpu {sound = max 0 (sound cpu - 1), delay = max 0 (delay cpu - 1)}


splitByte :: W.Word8 -> (W.Word8, W.Word8)
splitByte num = (n1, n2)
  where
    n1 = shiftR (num .&. 0xF0) 4
    n2 = num .&. 0xF


fetchOpcode :: CPU -> Opcode
fetchOpcode cpu = (a1, a2, b1, b2)
  where
    c = error $ show (a1, a2, b1, b2)
    (a1, a2) = splitByte $ memory cpu V.! (fromIntegral $ pc cpu)
    (b1, b2) = splitByte $ memory cpu V.! (fromIntegral $ pc cpu + 1)

prevOpcode :: CPU -> Opcode
prevOpcode cpu = fetchOpcode $ cpu { pc = pc cpu - 2}

glue2Nibbles :: W.Word8 -> W.Word8 -> W.Word8
glue2Nibbles a b = 
    shift a 4 + b

glue3Nibbles :: W.Word8 -> W.Word8 -> W.Word8 -> W.Word16
glue3Nibbles a b c = x + y + z
    where
        x = shift (fromIntegral a) 8
        y = shift (fromIntegral b) 4
        z = (fromIntegral c)

setReg :: CPU -> W.Word8 -> W.Word8 -> CPU
setReg cpu idx val =
    let nreg = (v cpu) V.// [((fromIntegral idx), val)] 
    in cpu {v = nreg}

getReg :: CPU -> W.Word8 -> W.Word8
getReg cpu x = (v cpu) V.! (fromIntegral x)

printOpcode :: Opcode -> String
printOpcode (0x0, 0x0, 0xE, 0x0) = "CLS"
printOpcode (0x1, a, b, c) = "JUMP " ++ (showHex (glue3Nibbles a b c) "")
printOpcode (0x2, a, b, c) = "CALL " ++ (showHex (glue3Nibbles a b c) "") 
printOpcode (0x0, 0x0, 0xE, 0xE) = "RET"
printOpcode (0x3, x, a, b) = "SKIP if V" ++ (showHex x "") ++ " = " ++ (showHex (glue2Nibbles a b) "")
printOpcode (0x4, x, a, b) = "SKIP if V" ++ (showHex x "") ++ " /= " ++ (showHex (glue2Nibbles a b) "")
printOpcode (0x5, x, y, 0x0) = "SKIP if V" ++ (showHex x "") ++ " == V" ++ (showHex y "")
printOpcode (0x9, x, y, 0x0) = "SKIP if V" ++ (showHex x "") ++ " /= V" ++ (showHex y "")
printOpcode (0x6, x, a, b) = "SET V" ++ (showHex x "") ++ " to " ++ (showHex (glue2Nibbles a b) "")
printOpcode (0x7, x, a, b) = "ADD V" ++ (showHex x "") ++ " to " ++ (showHex (glue2Nibbles a b) "")
printOpcode (0x8, x, y, 0x0) = "SET V" ++ (showHex x "") ++ "to V" ++ (showHex y "")
printOpcode (0x8, x, y, 0x1) = "OR V" ++ (showHex x "") ++ "and V" ++ (showHex y "")
printOpcode (0x8, x, y, 0x2) = "AND V" ++ (showHex x "") ++ "and V" ++ (showHex y "")
printOpcode (0x8, x, y, 0x3) = "XOR V" ++ (showHex x "") ++ "and V" ++ (showHex y "")
printOpcode (0x8, x, y, 0x4) = "ADD V" ++ (showHex x "") ++ "and V" ++ (showHex y "")
printOpcode (0x8, x, y, 0x5) = "SUB V" ++ (showHex x "") ++ "and V" ++ (showHex y "")
printOpcode (0x8, x, y, 0x7) = "SUB V" ++ (showHex y "") ++ "and V" ++ (showHex x "")
printOpcode (0x8, x, y, 0x6) = "SHIFTR V" ++ (showHex x "")
printOpcode (0x8, x, y, 0xE) = "SHIFTL V" ++ (showHex x "")
printOpcode (0xA, a, b, c) = "SET I to " ++ (showHex (glue3Nibbles a b c) "") 
printOpcode (0xB, a, b, c) = "REL JUMP by " ++ (showHex (glue3Nibbles a b c) "")
printOpcode (0xC, x, a, b) = "RAND V" ++ (showHex x "")
printOpcode (0xD, x, y, _) = "DRAW at x: V" ++ (showHex x "") ++ " y: V" ++ (showHex y "")
printOpcode (0xE, x, 0x9, 0xE) = "SKIP if V" ++ (showHex x "") 
printOpcode (0xE, x, 0xA, 0x1) = "SKIP if not V" ++ (showHex x "")
printOpcode (0xF, x, 0x0, 0x7) = "GET DELAY V" ++ (showHex x "")
printOpcode (0xF, x, 0x1, 0x5) = "SET DELAY V" ++ (showHex x "")
printOpcode (0xF, x, 0x1, 0x8) = "SET SOUND V" ++ (showHex x "")
printOpcode (0xF, x, 0x1, 0xE) = "ADD to I V" ++ (showHex x "")
printOpcode (0xF, x, 0x2, 0x9) = "LETTER V" ++ (showHex x "")
printOpcode (0xF, x, 0x0, 0xA) = "WAIT V" ++ (showHex x "")
printOpcode (0xF, x, 0x3, 0x3) = "DECIMAL V" ++ (showHex x "")
printOpcode (0xF, x, 0x5, 0x5) = "STORE REG V" ++ (showHex x "")
printOpcode (0xF, x, 0x6, 0x5) = "LOAD REG V" ++ (showHex x "")
printOpcode _ = showHex (glue3Nibbles 0x2 0x1 0x8) ""

-- trace ((printOpcode opcode) ++ " " ++ (show (sp cpu)) ++ " " ++ (show (pc cpu)) ++ " " ++ show (stack cpu)) $

execOpcode :: CPU -> Opcode -> CPU
execOpcode cpu opcode =
    case opcode of
        --cls
        (0x0, 0x0, 0xE, 0x0) ->
            incPC $ cpu {vram = emptyScreen}
        --jump
        (0x1, a, b, c) ->
            jump cpu $ glue3Nibbles a b c
        --call subroutine
        (0x2, a, b, c) ->
            jump (pushStack cpu) $ glue3Nibbles a b c
        -- return
        (0x0, 0x0, 0xE, 0xE) ->
            let (retaddr, ncpu) = popStack cpu
            in jump ncpu retaddr
        -- skip instr if vx = ab
        (0x3, x, a, b) ->
            let vx = getReg cpu x
                ab = glue2Nibbles a b in
            if vx == ab
                then skipInstr cpu
                else incPC cpu
        -- skip instr if vx != ab
        (0x4, x, a, b) ->
            let vx = getReg cpu x 
                ab = glue2Nibbles a b in
            if vx /= ab
                then skipInstr cpu
                else incPC cpu
        -- skip instr if vx == vy
        (0x5, x, y, 0x0) ->
            let vx = getReg cpu x 
                vy = getReg cpu y in
            if vx == vy
                then skipInstr cpu
                else incPC cpu
        -- skip inst if vx != vy
        (0x9, x, y, 0x0) ->
            let vx = getReg cpu x
                vy = getReg cpu y in
            if vx /= vy
                then skipInstr cpu
                else incPC cpu
        -- set vx to ab
        (0x6, x, a, b) ->
            let val = glue2Nibbles a b in
            incPC $ setReg cpu x val
        -- add ab to vx
        (0x7, x, a, b) ->
            let vx = getReg cpu x
                ab = glue2Nibbles a b in
            incPC $ setReg cpu x (vx + ab)
        -- set vx to vy
        (0x8, x, y, 0x0) ->
            let vy = getReg cpu y in
            incPC $ setReg cpu x vy
        -- set vx to vx or vy
        (0x8, x, y, 0x1) ->
            let vx = getReg cpu x
                vy = getReg cpu y in
            incPC $ setReg cpu x (vx .|. vy)
        -- set vx to vx and vy
        (0x8, x, y, 0x2) ->
            let vx = getReg cpu x
                vy = getReg cpu y in
            incPC $ setReg cpu x (vx .&. vy)
        -- set vx to vx xor vy
        (0x8, x, y, 0x3) ->
            let vx = getReg cpu x
                vy = getReg cpu y in
            incPC $ setReg cpu x (vx `xor` vy)
        -- set vx to vx + vy
        (0x8, x, y, 0x4) ->
            incPC $ addReg cpu x y 
        -- set vx to vx - vy
        (0x8, x, y, 0x5) ->
            incPC $ subReg cpu x y
        -- set vx to vx - vy
        (0x8, x, y, 0x7) ->
            incPC $ subReg2 cpu x y
        -- shift Vx right
        (0x8, x, _, 0x6) ->
            incPC $ shiftRReg cpu x
        -- shift Vc left
        (0x8, x, _, 0xE) ->
            incPC $ shiftLReg cpu x
        -- set i to abc
        (0xA, a, b, c) ->
            let abc = glue3Nibbles a b c in
            incPC $ cpu { i = abc }
        -- jump to V0 + abc
        (0xB, a, b, c) -> 
            let v0 = fromIntegral $ getReg cpu 0 
                abc = glue3Nibbles a b c in
            incPC $ jump cpu (v0 + abc)
        -- generate random number, and it with ab and store it in Vx
        (0xC, x, a, b) ->
            let (rval, ncpu) = genRandom cpu
                ab = glue2Nibbles a b 
            in  incPC $ setReg ncpu x (ab .&. rval)
        -- draw a 8xn sprite from memory i at vx vy 
        (0xD, x, y, n) ->
            let vx = getReg cpu x
                vy = getReg cpu y in 
                incPC $ drawSprite cpu vx vy n
        -- skip next instr if key vx is pressed
        (0xE, x, 0x9, 0xE) -> 
            let vx = getReg cpu x in
                if isPressed cpu vx
                    then skipInstr cpu
                    else incPC cpu
        -- skip next instr if key vx is not pressed
        (0xE, x, 0xA, 0x1) ->
            let vx = getReg cpu x in
                if not $ isPressed cpu vx
                    then skipInstr cpu
                    else incPC cpu
        -- set vx to current value of timer
        (0xF, x, 0x0, 0x7) ->
            let t = (delay cpu) in
                incPC $ setReg cpu x t 
        -- set delay to vx
        (0xF, x, 0x1, 0x5) ->
            let vx = getReg cpu x in
            incPC $ cpu {delay = vx}
        -- set sound to vx
        (0xF, x, 0x1, 0x8) ->
            let vx = getReg cpu x in
            incPC $ cpu {sound = vx}
        -- add vx to i
        (0xF, x, 0x1, 0xE) ->
            let vx = getReg cpu x
                idx = (i cpu) in
            incPC $ cpu {i = (idx + (fromIntegral vx))}
        -- wait for input and save it to vx
        (0xF, x, 0x0, 0xA) ->
            checkInput cpu x
        -- set i to addr of the letter vx (eqv to i * 5)
        (0xF, x, 0x2, 0x9) ->
            let addr = 5 * (fromIntegral $ getReg cpu x) in
            incPC $ cpu { i = addr }
        -- push decimal digits of vx to i, i + 1 and i + 2
        (0xF, x, 0x3, 0x3) ->
            incPC $ storeDecimal cpu x
        -- store registers up to x in memory
        (0xF, x, 0x5, 0x5) ->
            incPC $ storeRegInMemory cpu x
        -- load registers from memory
        (0xF, x, 0x6, 0x5) ->
            incPC $ storeMemoryInReg cpu x
        instr -> 
            let idx = (pc cpu) in
            error $ "nieznana instrukcja: " ++ show instr ++ " at addr: " ++ show idx




incPC :: CPU -> CPU
incPC cpu = cpu {pc = pc cpu + 2}

skipInstr :: CPU -> CPU
skipInstr = incPC . incPC

jump :: CPU -> W.Word16 -> CPU
jump cpu addr = cpu {pc = addr}

pushStack :: CPU -> CPU
pushStack cpu = 
    let ns = (stack cpu) V.// [((fromIntegral $ sp cpu), (pc $ incPC cpu))]
        s = (sp cpu) + 1 
    in cpu {stack = ns, sp = s}

popStack :: CPU -> (W.Word16, CPU) 
popStack cpu = 
    let retaddr = (stack cpu) V.! (fromIntegral $ (sp cpu) - 1) 
        s = max 0 $ (fromIntegral (sp cpu) - 1)
        ncpu = cpu {sp = fromIntegral s} 
    in (retaddr, ncpu)
        
genRandom :: CPU -> (W.Word8, CPU)
genRandom cpu =
    let (rval, newgen) = randomR (0, 255) (rgen cpu)
        ncpu = cpu { rgen = newgen } 
    in (rval, ncpu)

isPressed :: CPU -> W.Word8 -> Bool
isPressed cpu x = (keyboard cpu) V.! (fromIntegral x)

toListBE :: Bits a => a -> [Bool]
toListBE n = map (testBit n) [7,6..0]

getSprite :: CPU -> W.Word8 -> W.Word8 -> W.Word8 -> [Bool]
getSprite cpu x y n = concatMap (take width . toListBE) (take height b)
    where
        lx = (fromIntegral $ x `mod` 64)
        ly = (fromIntegral $ y `mod` 32)
        len = fromIntegral n
        width = min (64 - lx) 8
        height = min (32 - ly) len
        idx = fromIntegral (i cpu)
        mem = (memory cpu)
        b = V.toList $ V.slice idx len mem 

getMemRegion :: CPU -> W.Word8 -> W.Word8 -> W.Word8 -> [(Int, Bool)]
getMemRegion cpu x y n = do
        r <- [ly..(ly + height - 1)]
        c <- [lx..(lx + width - 1)]
        let i = 64 * r + c
        return (i, screen V.! i)
    where
        lx = (fromIntegral $ x `mod` 64) :: Int 
        ly = (fromIntegral $ y `mod` 32) :: Int
        screen = (vram cpu)
        width = min (64 - lx) 8
        height = min (32 - ly) (fromIntegral n)

drawSprite :: CPU -> W.Word8 -> W.Word8 -> W.Word8 -> CPU
drawSprite cpu x y n = 
        cpu {vram = screen V.// toWrite }
    where 
        screen = (vram cpu)
        sprite = getSprite cpu x y n
        memreg = getMemRegion cpu x y n
        toWrite = zipWith (\(i, a) b -> (i, a `xor` b)) memreg sprite



checkInput :: CPU -> W.Word8 -> CPU
checkInput cpu x =
        case pressed of
            Nothing -> cpu
            Just i -> incPC $ setReg cpu x (fromIntegral $ fst i)
    where 
        keys = V.toList (keyboard cpu)
        pressed = find snd (zip [0..] keys)

storeDecimal :: CPU -> W.Word8 -> CPU
storeDecimal cpu x =
        cpu { memory = newmem }
    where
        idx = fromIntegral (i cpu)
        mem = (memory cpu)
        a2 = x `mod` 10
        a1 = (div x 10) `mod` 10
        a0 = (div x 100) `mod` 10
        a = [(idx, a0), (idx + 1, a1), (idx + 2, a2)]
        newmem = mem V.// a

storeRegs :: CPU -> W.Word8 -> CPU
storeRegs cpu x =
        cpu {memory = (mem V.// (zip xs regs))}
    where
        idx = fromIntegral (i cpu)
        mem = (memory cpu)
        x = fromIntegral x
        regs = take (x + 1) $ V.toList (v cpu)
        xs = map (idx+) [0..x]

storeRegInMemory :: CPU -> W.Word8 -> CPU
storeRegInMemory cpu x =
    cpu { memory = (mem `V.update` V.zip locations regs)}
    where 
        endReg = fromIntegral x + 1
        idx = fromIntegral $ (i cpu)
        mem = (memory cpu)
        locations = V.enumFromN idx (idx + endReg)
        regs = V.slice 0 endReg (v cpu)

storeMemoryInReg :: CPU -> W.Word8 -> CPU
storeMemoryInReg cpu x =
    cpu { v = (regs `V.update` V.zip locations mem)}
    where
        endReg = fromIntegral x + 1
        idx = fromIntegral $ (i cpu)
        regs = (v cpu)
        locations = V.enumFromN 0 endReg
        mem = V.slice idx endReg (memory cpu)


loadRegs :: CPU -> W.Word8 -> CPU
loadRegs cpu x =
        cpu {v = ((v cpu) V.// (zip [0..] vals))}
    where
        idx = fromIntegral (i cpu)
        mem = (memory cpu)
        x = fromIntegral x
        vals = V.toList (V.slice idx (x + 1) mem)
        

addReg :: CPU -> W.Word8 -> W.Word8 -> CPU
addReg cpu x y =
        cpu { v = (v cpu) V.// [(fromIntegral x, vx + vy), (0xF, carry)]}
    where
        vx = getReg cpu x
        vy = getReg cpu y
        carry = if (0xFF :: W.Word16) < (fromIntegral vx) + (fromIntegral vy) then 1 else 0

subReg :: CPU -> W.Word8 -> W.Word8 -> CPU
subReg cpu x y =
     cpu { v = (v cpu) V.// [(fromIntegral x, vx - vy), (0xF, borrow)]}
    where
        vx = getReg cpu x
        vy = getReg cpu y
        borrow = if vx < vy then 0 else 1 

subReg2 :: CPU -> W.Word8 -> W.Word8 -> CPU
subReg2 cpu x y =
    cpu { v = (v cpu) V.// [(fromIntegral x, vx - vy), (0xF, borrow)]}
    where
        vx = getReg cpu x
        vy = getReg cpu y
        borrow = if vx > vy then 0 else 1

shiftLReg :: CPU -> W.Word8 -> CPU
shiftLReg cpu x =
    cpu { v = (v cpu) V.// [(fromIntegral x, shiftL vx 1), (0xF, carry)]}
    where
        vx = getReg cpu x
        carry = shiftR vx 7 

shiftRReg :: CPU -> W.Word8 -> CPU
shiftRReg cpu x =
    cpu { v = (v cpu) V.// [(fromIntegral x, shiftR vx 1), (0xF, carry)]}
    where
        vx = getReg cpu x
        carry = vx .&. 1