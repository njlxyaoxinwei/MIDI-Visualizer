> {-# LANGUAGE Arrows #-}

> module Main where

> import Visualize.Play (controlPanel)
> import Visualize.Music (groupMsgs, midiToMsgs)
> import Visualize.Display (displayArrow, displaySys, ResetDisplay(ResetAll))
> import System.Environment (getArgs)
> import Euterpea

main is the entry point of the visualize program

> main :: IO ()
> main = do 
>   args <- getArgs
>   case args of 
>     []    -> putStrLn "No path specified!"
>     (x:_)-> do 
>       file <- importFile x
>       case file of
>         Left s    -> putStrLn s
>         Right mid -> visualize $ midiToMsgs mid

visualize takes an array of timed messages and start the MUI

> visualize :: [(DeltaT, Message)]->IO ()
> visualize msgs = msgs `seq` runMUI myMUIParams $ leftRight $ proc _ -> do 
>   (ms, rd) <- setSize (350,600) (leftPane msgs) -< ()
>   rightPane -< (ms, rd)
>   returnA   -< ()

leftPane consists of output selection, control Panel and System Information

> leftPane :: [(DeltaT, Message)]->UISF () ([[Message]], ResetDisplay)
> leftPane msgs = topDown $ proc _ -> do 
>   dev           <- selectOutput      -< ()
>   (ms, _ , rd)  <- controlPanel msgs -< dev
>   let (ss, cs) = groupMsgEvents ms
>   displaySys  -< (ss, rd==ResetAll)
>   returnA  -< (cs, rd)
>   where groupMsgEvents Nothing      = ([], replicate 16 [])
>         groupMsgEvents (Just msgs') = groupMsgs msgs'

rightPane consists of visualization of the channels.

> rightPane :: UISF ([[Message]], ResetDisplay) ()
> rightPane = topDown $ proc (msgs, rd) -> do 
>   displayArrow -< (msgs, rd)

MUI Params for visualize

> myMUIParams :: UIParams
> myMUIParams = defaultMUIParams{uiTitle="MIDI Visualizer",uiSize=(1400,600)}


