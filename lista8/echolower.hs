import Data.Char
import System.IO

echolower :: IO()
echolower = do
    b <- isEOF
    if b
    then 
        do
        c <- getChar
        putChar $ toLower c
        echolower
    else return ()



