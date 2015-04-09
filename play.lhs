> {-# LANGUAGE Arrows #-}

> module Visualize.Play where
> import Euterpea
> import Codec.Midi
> import Euterpea.IO.MUI.MidiWidgets
> import FRP.UISF.AuxFunctions


> playMid :: [(DeltaT, Message)]->IO ()
> playMid msgs = runMUI defaultMUIParams $ proc _ -> do 
>  playMidArrow msgs -< ()
>  returnA -< ()

> playMidArrow :: [(DeltaT, Message)]->UISF () (SEvent [Message])
> playMidArrow msgs = proc _ -> do 
>   dev<-selectOutput-<()
>   rec playing <- delay False -< playing'
>       e <- do if playing then edge<<<button "stop"-<()
>                          else edge<<<button "play"-<()
>       bop <- getBufferOp msgs -< (e, playing)
>       (maybeMsgs,playing') <- second (arr not)<<<eventBuffer -< bop
>   let midiMsgs = fmap (map Std) $ checkStop bop ~++ maybeMsgs 
>   midiOut-<(dev, midiMsgs)
>   returnA-<maybeMsgs
>   where checkStop bop = if shouldClearBuffer bop then Just (stopAllNotes [0..15]) else Just []

> shouldClearBuffer :: BufferOperation a->Bool
> shouldClearBuffer bop = case bop of
>   SetBufferPlayStatus _ b -> shouldClearBuffer b
>   SetBufferTempo      _ b -> shouldClearBuffer b
>   ClearBuffer             -> True
>   SkipAheadInBuffer   _   -> True
>   _                       -> False

> stopAllNotes :: [Channel]->[Message]
> stopAllNotes cs = map (\c->ControlChange c 123 0) cs

> getBufferOp :: [(DeltaT, Message)]->UISF (SEvent (), Bool) (BufferOperation Message)
> getBufferOp msgs = proc (click, p) -> do 
>                      case click of 
>                        Nothing -> returnA -< NoBOp
>                        Just _ -> do 
>                          if p then returnA -< ClearBuffer
>                          else returnA -< AppendToBuffer msgs



