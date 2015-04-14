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
>         Right mid -> visualize $ midiToMsgs mid

Takes an array of timed messages and perform visualize.

> visualize :: [(DeltaT, Message)]->IO ()
> visualize msgs = msgs `seq` runMUI myMUIParams $ leftRight $ proc _ -> do 
>   (ms, rd) <- leftPane msgs -< ()
>   rightPane -< (ms, rd)
>   returnA   -< ()

> leftPane :: [(DeltaT, Message)]->UISF () ([[Message]], ResetDisplay)
> leftPane msgs = topDown $ proc _ -> do 
>   dev           <- selectOutput      -< ()
>   (ms, ps, rd)  <- controlPanel msgs -< dev
>   let (ss, cs) = groupMsgEvents ms
>   displaySys  -< (ss, rd==ResetAll)
>   returnA  -< (cs, rd)

> rightPane :: UISF ([[Message]], ResetDisplay) ()
> rightPane = topDown $ proc (msgs, rd) -> do 
>   displayArrow -< (msgs, rd)

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

