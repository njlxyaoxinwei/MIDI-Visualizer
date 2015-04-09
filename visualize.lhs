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

> debugMidi mid = (fileType mid, timeDiv mid, map (filter myFilter) $ tracks mid) where
>   myFilter (_, NoteOn _ _ _) = False
>   myFilter (_, NoteOff _ _ _) = False
>   myFilter (_, Reserved _ _) = False
>   myFilter (_, Text _) = False
>   myFilter (_, ControlChange _ _ _) = False
>   myFilter (_, ProgramChange _ _) = True
>   myFilter (_, TempoChange _) = True
>   myFilter _ = False

