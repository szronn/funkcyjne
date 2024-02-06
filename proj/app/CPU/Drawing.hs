module CPU.Drawing where  

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import CPU.CPU
import CPU.Emulate
import qualified Data.ByteString as B
import qualified Data.Word as W
import qualified Data.Vector as V
import Numeric (showHex)
import Debug.Trace

createFrame :: V.Vector Bool  -> Picture
createFrame v = bitmapOfByteString 64 32 (BitmapFormat TopToBottom PxRGBA) pixels False
    where 
        pixels = B.pack $ concatMap f (V.toList v)
        f False = [0, 0, 0, 255]
        f True = [255, 255, 255, 255]

render :: (Int, Int) -> CPU -> Picture 
render (x, y) cpu = scale (realToFrac x / 64) (realToFrac y / 32) $ createFrame (vram cpu)

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


onInput :: Event -> CPU -> CPU
onInput (EventKey (Char key) Up _ _) cpu = unPressKey key cpu
onInput (EventKey (Char key) Down _ _) cpu = pressKey key cpu
onInput _ cpu = cpu

onInputStep :: Event -> CPU -> CPU
onInputStep (EventKey (Char 'p') Down _ _) cpu = trace (showRegs cpu) (clearKeyboard $ tick cpu)
onInputStep (EventKey (Char 'o') Down _ _) cpu = trace (showRegs cpu) (clearKeyboard $ dekaTick cpu)
onInputStep (EventKey (Char 'l') Down _ _) cpu = trace (showRegs cpu) (clearKeyboard $ hectaTick cpu)    
onInputStep (EventKey (Char key) Down _ _) cpu = pressKey key cpu
onInputStep _ cpu = cpu

showRegs :: CPU -> String
showRegs cpu = "registers: " ++ regs ++ "\n" ++ " I: " ++ idx ++ " PC: " ++ cnt ++ " " ++ instr
  where
    regs = show $ map (flip showHex "") $ V.toList (v cpu)
    idx = showHex (i cpu) ""
    cnt = showHex (pc cpu) ""
    instr = printOpcode $ fetchOpcode cpu 

startEmu :: (Int, Int) -> Int -> CPU -> IO ()
startEmu xy fps cpu = play FullScreen white fps cpu (render xy) onInput (\_ -> tick)

window = InWindow "Step by Step" (800, 800) (100, 100)

stepEmu :: (Int, Int) -> Int -> CPU -> IO ()
stepEmu xy fps cpu = play window white fps cpu (render xy) onInputStep (\_ -> id)