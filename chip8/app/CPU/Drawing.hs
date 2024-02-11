module CPU.Drawing where  

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import CPU.CPU
import CPU.Emulate
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Numeric (showHex)
import System.Exit

createFrame :: V.Vector Bool  -> Picture
createFrame screen = bitmapOfByteString 64 32 (BitmapFormat TopToBottom PxRGBA) pixels False
    where 
        pixels = B.pack $ concatMap f (V.toList screen)
        f False = [0, 0, 0, 255]
        f True = [255, 255, 255, 255]


render :: (Int, Int) -> CPU -> Picture 
render (x, y) cpu = scale (realToFrac x / 64) (realToFrac y / 32) $ createFrame (vram cpu)


renderIO :: (Int, Int) -> CPU -> IO Picture
renderIO xy cpu = return $ render xy cpu

decodeKey :: Char -> Maybe Int
decodeKey '1' = Just 1
decodeKey '2' = Just 2
decodeKey '3' = Just 3
decodeKey '4' = Just 0xC
decodeKey 'q' = Just 4
decodeKey 'w' = Just 5
decodeKey 'e' = Just 6
decodeKey 'r' = Just 0xD
decodeKey 'a' = Just 7
decodeKey 's' = Just 8
decodeKey 'd' = Just 9
decodeKey 'f' = Just 0xE
decodeKey 'z' = Just 0xA
decodeKey 'x' = Just 0
decodeKey 'c' = Just 0xB
decodeKey 'v' = Just 0xF
decodeKey _ = Nothing

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
printOpcode (0x8, x, _, 0x6) = "SHIFTR V" ++ (showHex x "")
printOpcode (0x8, x, _, 0xE) = "SHIFTL V" ++ (showHex x "")
printOpcode (0xA, a, b, c) = "SET I to " ++ (showHex (glue3Nibbles a b c) "") 
printOpcode (0xB, a, b, c) = "REL JUMP by " ++ (showHex (glue3Nibbles a b c) "")
printOpcode (0xC, x, _, _) = "RAND V" ++ (showHex x "")
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
printOpcode (a, b, c, d) = "(" ++ showHex a " " ++ showHex b " " ++ showHex c " " ++ showHex d "" ++ ")"  

pressKey :: Char -> CPU -> CPU
pressKey key cpu =
    case decodeKey key of
        Nothing -> cpu
        Just n -> cpu {keyboard = (keyboard cpu) V.// [(n, True)]}

unPressKey :: Char -> CPU -> CPU
unPressKey key cpu =
    case decodeKey key of
        Nothing -> cpu
        Just n -> cpu {keyboard = (keyboard cpu) V.// [(n, False)]}


onInput :: Event -> CPU -> IO CPU
onInput (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
onInput (EventKey (Char key) Up _ _) cpu = return $ unPressKey key cpu
onInput (EventKey (Char key) Down _ _) cpu = return $ pressKey key cpu
onInput _ cpu = return cpu

onInputStep :: Event -> CPU -> IO CPU
onInputStep (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
onInputStep (EventKey (Char 'p') Down _ _) cpu = return $ (clearKeyboard $ tick cpu)
onInputStep (EventKey (Char 'o') Down _ _) cpu = return $ (clearKeyboard $ dekaTick cpu)
onInputStep (EventKey (Char 'l') Down _ _) cpu = return $ (clearKeyboard $ hectaTick cpu)
onInputStep (EventKey (Char 'i') Down _ _) cpu = printDebug cpu >> return cpu    
onInputStep (EventKey (Char key) Down _ _) cpu = return $ pressKey key cpu
onInputStep _ cpu = return cpu

showRegs :: CPU -> String
showRegs cpu = show $ map (`showHex` "") $ V.toList (v cpu)

showPC :: CPU -> String
showPC cpu = showHex (pc cpu) "" 

showI :: CPU -> String
showI cpu = showHex (i cpu) ""

showOpcode :: CPU -> String
showOpcode cpu = printOpcode $ fetchOpcode cpu

printDebug :: CPU -> IO ()
printDebug cpu = do
    putStrLn $ showRegs cpu
    putStrLn $ "I: " ++ showI cpu ++ " PC: " ++ showPC cpu
    putStrLn $ showOpcode cpu



-- showRegs :: CPU -> String
-- showRegs cpu = "registers: " ++ regs ++ "\n" ++ " I: " ++ idx ++ " PC: " ++ cnt ++ " " ++ instr
--   where
--     regs = show $ map (flip showHex "") $ V.toList (v cpu)
--     idx = showHex (i cpu) ""
--     cnt = showHex (pc cpu) ""
--     instr = printOpcode $ fetchOpcode cpu 


window :: Display
window = InWindow "Step by Step" (800, 800) (100, 100)

startEmu :: (Int, Int) -> Int -> CPU -> IO ()
startEmu xy fps cpu = playIO FullScreen white fps cpu (renderIO xy) onInput (const $ return . tick)

stepEmu :: (Int, Int) -> Int -> CPU -> IO ()
stepEmu xy fps cpu = playIO window white fps cpu (renderIO xy) onInputStep (const $ return . id)