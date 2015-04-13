> {-# LANGUAGE Arrows #-}

> module Visualize.Play where
> import Euterpea
> import Codec.Midi
> import Euterpea.IO.MUI.MidiWidgets
> import FRP.UISF.AuxFunctions

PlayStatus is the status of the player. PlayEvent is the event of changing the 
play status

> data PlayStatus = Playing | PStopped | Paused deriving (Show, Eq)
> data PlayEvent  = Play | PStop | Pause | PResume | PSkip DeltaT deriving (Show, Eq)

playMidArrow takes an array of timed midi messages and creates a MUI that has a 
player UI, including buttons for play/resume/stop/skip-ahead and sliders for the 
amount to skip-ahead as well as for the playback speed. It generates a stream of 
midi message events that are occurring at each time slot.

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

Certain BufferOperation, when applied to the buffer, requires notes on all 
channels to be stopped at once. 

> shouldClearBuffer :: BufferOperation a->Bool
> shouldClearBuffer bop = case bop of
>   SetBufferPlayStatus True b  -> shouldClearBuffer b
>   SetBufferPlayStatus False _ -> True
>   SetBufferTempo      _ b     -> shouldClearBuffer b
>   ClearBuffer                 -> True
>   SkipAheadInBuffer   _       -> True
>   _                           -> False

> tempoSlider :: UISF () Double
> tempoSlider = withCustomDisplay "x" $ mySlider 10 ((1/10),10) 1<<<label "Playback Speed"

Button controls for playMidArrow

> playButtons :: UISF PlayStatus (SEvent PlayEvent)
> playButtons = proc ps -> do 
>   if ps == PStopped 
>     then fmap (const Play) ^<< edge<<<button "play"-<()
>     else do dt<-label "Skip Ahead">>>withCustomDisplay " seconds" (mySlider 10 ((1/10), 30) 1)-<()
>             (| leftRight ( do 
>                 e1 <- do case ps of 
>                            Playing -> edge<<<button "pause" -<()
>                            Paused  -> edge<<<button "resume"-<()
>                 e2 <- edge<<<button "stop" -< ()   
>                 e3 <- edge<<<button ">>"   -< ()
>                 returnA -< helper dt (helper2 ps) (e1,e2,e3)
>              ) |)
>   where helper dt pe es = case es of 
>                         (_,Just _,_) -> Just PStop
>                         (Just _,_,_) -> Just pe
>                         (_,_,Just _) -> Just (PSkip dt)      
>                         _            -> Nothing
>         helper2 Playing = Pause
>         helper2 Paused  = PResume

Given the timed messages to play, creates a arrow that returns the appropriate
BufferOperation and the updated PlayStatus according to the PlayStatus and 
PlayEvent at each time slot.

> getBOp :: [(DeltaT, Message)]->UISF (PlayStatus, SEvent PlayEvent) (BufferOperation Message, PlayStatus)
> getBOp msgs = proc (ps, pe) -> do 
>   case (ps,pe) of
>     (PStopped,  Just Play)-> returnA -< (SetBufferPlayStatus True $ AppendToBuffer msgs, Playing)
>     (Playing,  Just Pause)-> returnA -< (SetBufferPlayStatus False NoBOp, Paused)
>     (Paused, Just PResume)-> returnA -< (SetBufferPlayStatus True NoBOp, Playing)
>     (_,        Just PStop)-> returnA -< (ClearBuffer, PStopped)
>     (_,    Just (PSkip x))-> returnA -< (SkipAheadInBuffer x, ps)
>     _                     -> returnA -< (NoBOp, ps)


Generates the messages for stopping all notes (control number 123) on the given
channels.

> stopAllNotes :: [Channel]->[Message]
> stopAllNotes cs = map (\c->ControlChange c 123 0) cs

Like withDisplay but allows an additional string to be appended.

> withCustomDisplay :: (Show b)=>String->UISF a b->UISF a b
> withCustomDisplay str arrow = (arrow >>^ id &&& (++str).show) >>> second displayStr >>^ fst

A Slider for rationals, essentially a wrapper around hiSlider.

> mySlider :: Int->(Rational, Rational)->Rational->UISF () Double
> mySlider scale (low, high) def = let [low', high', def'] = map (truncate.(*(fromIntegral scale))) [low, high, def]
>                                      intSlider = hiSlider scale (low', high') def'
>                                  in intSlider>>^((/fromIntegral scale).fromIntegral)
