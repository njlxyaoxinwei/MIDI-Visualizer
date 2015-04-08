> module Main where

> import Visualize.Play
> import Visualize.Music
> import System.Environment
> import Codec.Midi

> main = do 
>   args <- getArgs
>   case args of 
>     []    -> putStrLn "No path specified!"
>     (x:xs)-> do 
>       file <- importFile x
>       case file of
>         Left s    -> putStrLn s
>         Right mid -> (putStrLn . show . debugMidi $ mid) >> (playMid $ midiToMsgs mid)

> debugMidi mid = (fileType mid, timeDiv mid, map (filter notNote) $ tracks mid) where
>   notNote (_, NoteOn _ _ _) = False
>   notNote (_, NoteOff _ _ _) = False
>   notNote (_, Reserved _ _) = False
>   notNote _ = True

