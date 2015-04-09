> {-# LANGUAGE Arrows #-}

> module Visualize.Play where
> import Euterpea
> import Codec.Midi
> import Euterpea.IO.MUI.MidiWidgets
> import FRP.UISF.AuxFunctions


> playMidArrow :: [(DeltaT, Message)]->UISF () (SEvent [Message])
> playMidArrow msgs = proc _ -> do 
>   dev<-selectOutput-<()
>   rec pStatus <- delay PStopped -< pStatus''
>       pe <- playButtons -< pStatus
>       (bop, pStatus') <- getBOp msgs -< (pStatus, pe)
>       (maybeMsgs, isEmpty) <- eventBuffer -< bop
>       let pStatus'' = if isEmpty then PStopped else pStatus'
>   midiOut-<(dev, fmap (map Std) $ checkStop bop ~++ maybeMsgs)
>   returnA-<maybeMsgs
>   where checkStop bop = if shouldClearBuffer bop then Just (stopAllNotes [0..15]) else Just []

> shouldClearBuffer :: BufferOperation a->Bool
> shouldClearBuffer bop = case bop of
>   SetBufferPlayStatus _ b -> shouldClearBuffer b
>   SetBufferTempo      _ b -> shouldClearBuffer b
>   ClearBuffer             -> True
>   SkipAheadInBuffer   _   -> True
>   _                       -> False

arr helper Pause <<<(edge<<<button " ") &&& (edge<<<button " ")-<()

> playButtons :: UISF PlayStatus (SEvent PlayEvent)
> playButtons = leftRight $ proc ps -> do 
>   case ps of
>     Playing  -> arr (helper Pause) <<< (edge<<<button "pause") &&& (edge<<<button "stop")   -<()
>     Paused   -> arr (helper PResume) <<< (edge<<<button "resume") &&& (edge<<<button "stop")-<()
>     PStopped -> arr (fmap (const Play)) <<< edge<<<button "play"                          -<()
>   where helper pe es = case es of 
>                         (_, Just _)       -> Just PStop
>                         (Just _, _)       -> Just pe
>                         (Nothing, Nothing)-> Nothing

> data PlayStatus = Playing | PStopped | Paused
> data PlayEvent  = Play | PStop | Pause | PResume

> getBOp :: [(DeltaT, Message)]->UISF (PlayStatus, SEvent PlayEvent) (BufferOperation Message, PlayStatus)
> getBOp msgs = proc (ps, pe) -> do 
>   case (ps,pe) of
>     (_,           Nothing)-> returnA -< (NoBOp, ps)
>     (PStopped,  Just Play)-> returnA -< (SetBufferPlayStatus True $ AppendToBuffer msgs, Playing)
>     (Playing,  Just Pause)-> returnA -< (SetBufferPlayStatus False NoBOp, Paused)
>     (Paused, Just PResume)-> returnA -< (SetBufferPlayStatus True NoBOp, Playing)
>     (_,        Just PStop)-> returnA -< (ClearBuffer, PStopped)


> stopAllNotes :: [Channel]->[Message]
> stopAllNotes cs = map (\c->ControlChange c 123 0) cs

> getBufferOp :: [(DeltaT, Message)]->UISF (SEvent (), Bool) (BufferOperation Message)
> getBufferOp msgs = proc (click, p) -> do 
>                      case click of 
>                        Nothing -> returnA -< NoBOp
>                        Just _ -> do 
>                          if p then returnA -< ClearBuffer
>                          else returnA -< AppendToBuffer msgs



