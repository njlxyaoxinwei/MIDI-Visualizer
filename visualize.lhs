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
>         Right mid -> putStrLn (show mid) >> (playMid $ midiToMsgs mid)


