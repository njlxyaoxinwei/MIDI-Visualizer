> {-# LANGUAGE Arrows #-}

> module Visualize.Play where
> import Euterpea
> import Codec.Midi
> import Euterpea.IO.MUI.MidiWidgets
> import FRP.UISF.AuxFunctions

> data PlayStatus = Playing | PStopped | Paused
> data PlayEvent  = Play | PStop | Pause | PResume | PSkip DeltaT

> playMidArrow :: [(DeltaT, Message)]->UISF () (SEvent [Message])
> playMidArrow msgs = proc _ -> do 
>   dev<-selectOutput-<()
>   rec pStatus <- delay PStopped -< pStatus''
>       pe <- playButtons -< pStatus
>       (bop, pStatus') <- getBOp msgs -< (pStatus, pe)
>       tp<-unique<<< tempoSlider -<()
>       (maybeMsgs, isEmpty) <- eventBuffer -< maybe bop (\x->SetBufferTempo x bop) tp
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


> tempoSlider :: UISF () Double
> tempoSlider = withDisplay $ ((/10).fromIntegral)^<<hiSlider 10 (1,100) 10<<<label "Playback Speed"

> playButtons :: UISF PlayStatus (SEvent PlayEvent)
> playButtons = proc ps -> do 
>   case ps of
>     Playing  -> do dt<-label "Skip Ahead">>>withDisplay (hSlider (0.1,30) 1)-<()
>                    (| leftRight ( do e1<-edge<<<button "pause"-< ()
>                                      e2<-edge<<<button "stop" -< ()
>                                      e3<-edge<<<button ">>"   -< ()
>                                      returnA -< helper dt Pause (e1,e2,e3))|)
>     Paused   -> do dt<-label "Skip Ahead">>>withDisplay (hSlider (0.1,30) 1)-<()
>                    (| leftRight ( do e1<-edge<<<button "resume"-< ()
>                                      e2<-edge<<<button "stop" -< ()
>                                      e3<-edge<<<button ">>"   -< ()
>                                      returnA -< helper dt PResume (e1,e2,e3))|)
>     PStopped -> fmap (const Play) ^<< edge<<<button "play"-<()
>   where helper dt pe es = case es of 
>                         (_,Just _,_) -> Just PStop
>                         (Just _,_,_) -> Just pe
>                         (_,_,Just _) -> Just (PSkip dt)      
>                         _            -> Nothing


> getBOp :: [(DeltaT, Message)]->UISF (PlayStatus, SEvent PlayEvent) (BufferOperation Message, PlayStatus)
> getBOp msgs = proc (ps, pe) -> do 
>   case (ps,pe) of
>     (PStopped,  Just Play)-> returnA -< (SetBufferPlayStatus True $ AppendToBuffer msgs, Playing)
>     (Playing,  Just Pause)-> returnA -< (SetBufferPlayStatus False NoBOp, Paused)
>     (Paused, Just PResume)-> returnA -< (SetBufferPlayStatus True NoBOp, Playing)
>     (_,        Just PStop)-> returnA -< (ClearBuffer, PStopped)
>     (_,    Just (PSkip x))-> returnA -< (SkipAheadInBuffer x, ps)
>     _                     -> returnA -< (NoBOp, ps)



> stopAllNotes :: [Channel]->[Message]
> stopAllNotes cs = map (\c->ControlChange c 123 0) cs

