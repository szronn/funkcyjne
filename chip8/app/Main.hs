module Main (main) where

import Graphics.Gloss.Interface.Environment (getScreenSize)

import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.Word as W
import CPU.CPU
import CPU.Drawing
import System.Random
import System.Environment

data Mode = Run | Debug

getRom :: FilePath -> IO (V.Vector W.Word8)
getRom path = do
    file <- B.readFile path
    let rom = V.fromList $ B.unpack file
    let len = V.length rom
    if len > 0xE00 || odd len
        then error "niepoprawny rom"
        else return rom

main :: IO ()
main = do
    args <- getArgs
    run args 
   

runEmu :: String -> Int -> Mode -> IO ()
runEmu name speed Run = do
    rom <- getRom name
    size <- getScreenSize
    rng <- getStdGen
    let cpu = defaultCPU rom rng
    startEmu size speed cpu
runEmu name speed Debug = do
    rom <- getRom name
    size <- getScreenSize
    rng <- getStdGen
    let cpu = defaultCPU rom rng
    stepEmu size speed cpu

run :: [String] -> IO ()
run [] = putStrLn "nie podałeś ścieżki do romu"
run [name] = runEmu name 100 Run
run [name, s] = runEmu name (read s) Run
run [name, s, "RUN"] = runEmu name (read s) Run
run [name, s, "DEBUG"] = runEmu name (read s) Debug
run _ = putStrLn "tryb uruchomienia może być tylko RUN albo DEBUG" 


