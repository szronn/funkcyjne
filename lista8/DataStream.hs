{-# LANGUAGE LambdaCase #-}

import System.IO
import Data.Char
import Control.Applicative
import Control.Monad



data StreamTrans i o a 
    = Return a 
    | ReadS (Maybe i -> StreamTrans i o a) 
    | WriteS o (StreamTrans i o a)

echoLower :: StreamTrans Char Char ()

echoLower = ReadS $ \i -> 
    case i of
        Nothing -> Return ()
        Just i -> WriteS (Data.Char.toLower i) echoLower

echoUpper:: StreamTrans Char Char ()
echoUpper = ReadS $ \case
        Nothing -> Return ()
        Just i -> WriteS (Data.Char.toUpper i) echoUpper


runIOStreamTrans :: StreamTrans Char Char a -> IO a
runIOStreamTrans stream =
    case stream of
        Return a        -> return a
        ReadS f         -> do
            b <- isEOF
            if b
            then runIOStreamTrans $ f Nothing
            else 
                do
                    c <- getChar
                    runIOStreamTrans $ f (Just c)
        WriteS c stream -> do
            putChar c
            runIOStreamTrans stream


listTrans :: StreamTrans i o a -> [i] -> ([o], a)
listTrans stream input = 
    case stream of
        Return a -> ([], a)
        ReadS f -> 
            case input of
                i : input -> listTrans (f (Just i)) input
                [] -> listTrans (f Nothing) []
        WriteS o stream ->
            let (os, a) = listTrans stream input in
                (o : os, a)

-- kocham leniwość (tak naprawde to nie)
test :: [Char]
test = take 3 $ fst $ listTrans echoLower ['A'..]

-- runCycle st = b
--     where (s, b) = listTrans st s

runCycle :: StreamTrans a a b -> b
runCycle stream = inner stream []
    where inner stream xs = 
            case stream of
                Return b -> b
                ReadS f  ->
                    case xs of
                        x : xs -> inner (f (Just x)) xs
                        [] -> inner (f Nothing) []
                WriteS a stream -> inner stream (a : xs)




(|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b
left |>| right = 
    case left of
        Return a -> noInput right
        ReadS f -> 
            ReadS $ \i -> f i |>| right
        WriteS m left -> 
            case right of
                Return b       -> Return b
                ReadS g        -> left `helper` g (Just m)
                WriteS o right -> WriteS o $ left |>| right  
        where
            helper left right =
                case right of
                    Return b -> Return b
                    ReadS g  -> left |>| g Nothing
                    WriteS o right -> WriteS o $ left |>| right  
            noInput stream =
                case stream of
                    Return b -> Return b
                    ReadS f -> noInput $ f Nothing
                    WriteS o stream -> WriteS o $ noInput stream


withInputs :: StreamTrans i o a -> [i] -> StreamTrans b o a
withInputs stream [] =
    case stream of
        Return a -> Return a
        ReadS f -> withInputs (f Nothing) []
        m -> withInputs m []

withInputs stream (x:xs) =
    case stream of
        Return a -> Return a
        ReadS f -> withInputs (f (Just x)) xs
        m -> withInputs m (x:xs)


catchOutput :: StreamTrans i o a -> StreamTrans i b (a, [o])
catchOutput stream = inner stream []
    where
        inner stream xs =
            case stream of
                Return a -> Return (a, reverse xs)
                WriteS o stream -> inner stream (o:xs)
                ReadS f -> ReadS $ \x ->  inner (f x) xs



instance Functor (StreamTrans i o) where
    fmap f stream = stream >>= (return . f)

instance Applicative (StreamTrans i o) where
  pure = Return
  (<*>) = ap

instance Monad (StreamTrans i o) where
    return = pure
    (>>=) :: StreamTrans i o a -> (a -> StreamTrans i o b) -> StreamTrans i o b
    Return a >>= f = f a
    ReadS g >>= f = ReadS $ \i -> g i >>= f
    WriteS o stream >>= f = WriteS o $ stream >>= f 




