> {-# LANGUAGE Arrows #-}

> module Visualize.Play where
> import Euterpea
> import Codec.Midi
> import Euterpea.IO.MUI.MidiWidgets
> import FRP.UISF.AuxFunctions


> playMid :: [(DeltaT, MidiMessage)]->IO ()
> playMid = runMUI defaultMUIParams . playMidArrow

> playMidArrow :: [(DeltaT, MidiMessage)]->UISF () ()
> playMidArrow msgs = proc _ -> do 
>   dev<-selectOutput-<()
>   rec playing <- delay False -< playing'
>       e <- do if playing then edge<<<button "stop"-<()
>                          else edge<<<button "play"-<()
>       bop <- getBufferOp msgs -< (e, playing)
>       (maybeMsgs,isemp) <- eventBuffer -< bop
>       midiOut-<(dev, if shouldClearBuffer bop 
>                      then Just (stopAllNotes [0..15]) ~++ maybeMsgs
>                      else maybeMsgs)
>       let playing' = not isemp
>   returnA-<()

> shouldClearBuffer :: BufferOperation a->Bool
> shouldClearBuffer bop = case bop of
>   SetBufferPlayStatus _ b -> shouldClearBuffer b
>   SetBufferTempo      _ b -> shouldClearBuffer b
>   ClearBuffer             -> True
>   SkipAheadInBuffer   _   -> True
>   _                       -> False

> stopAllNotes :: [Channel]->[MidiMessage]
> stopAllNotes cs = map (\c->Std (ControlChange c 123 0)) cs

> getBufferOp :: [(DeltaT, MidiMessage)]->UISF (SEvent (), Bool) (BufferOperation MidiMessage)
> getBufferOp msgs = proc (e, p) -> do 
>                      case e of 
>                        Nothing -> returnA -< NoBOp
>                        Just x -> do 
>                          if p then returnA -< ClearBuffer
>                          else returnA -< AppendToBuffer msgs



