> {-# LANGUAGE Arrows #-}

> module Main where

> import Visualize.Play
> import Visualize.Music
> import Visualize.Display
> import System.Environment
> import Codec.Midi
> import Euterpea

Entry point

> main = do 
>   args <- getArgs
>   case args of 
>     []    -> putStrLn "No path specified!"
>     (x:xs)-> do 
>       file <- importFile x
>       case file of
>         Left s    -> putStrLn s
>         Right mid -> (putStrLn . show . debugMidi $ mid) >> (visualize $ midiToMsgs mid)

Takes an array of timed messages and perform visualize.

> visualize :: [(DeltaT, Message)]->IO ()
> visualize msgs = runMUI myMUIParams $ proc _ -> do 
>   (ms, ps) <- playMidArrow msgs -< ()
>   let (ss, cs) = groupMsgEvents ms
>   displaySys        -< ss
>   displayMessages   -< ss
>   displayArrow      -< cs
>   returnA -< ()

MUI Params

> myMUIParams = defaultMUIParams{uiSize=(800,900)}

================================================================================
For debugging purposes

> debugMidi mid = (fileType mid, timeDiv mid, map (filter myFilter) $ tracks mid) where
>   myFilter (_, NoteOn _ _ _) = False
>   myFilter (_, NoteOff _ _ _) = False
>   myFilter (_, Reserved _ _) = False
>   myFilter (_, Text _) = False
>   myFilter (_, ControlChange _ _ _) = False
>   myFilter (_, ProgramChange _ _) = True
>   myFilter (_, TempoChange _) = True
>   myFilter _ = False

