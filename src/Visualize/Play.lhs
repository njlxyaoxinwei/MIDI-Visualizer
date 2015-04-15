> {-# LANGUAGE Arrows #-}

> module Visualize.Play (
>   PlayStatus(..), PlayEvent(..),
>   controlPanel, 
>   playButtons, getBOp, 
>   mySlider, withCustomDisplay, tempoSlider, deltaTSlider,
>   stopAllNotes
>  ) where
> import Visualize.Display (ResetDisplay(..))
> import Euterpea
> import Codec.Midi (Message, Channel)
> import FRP.UISF.AuxFunctions (eventBuffer)

PlayStatus is the status of the player. PlayEvent is the event of changing the 
play status

> data PlayStatus = Playing | PStopped | Paused deriving (Show, Eq)
> data PlayEvent  = Play | PStop | Pause | PResume | PSkip DeltaT deriving (Show, Eq)

> controlPanel :: [(DeltaT, Message)]->UISF (Maybe OutputDeviceID) (SEvent [Message], PlayStatus, ResetDisplay)
> controlPanel msgs = title "Control" $ proc dev -> do 
>   rec pStatus             <- delay PStopped       -< pStatus''
>       pe                  <- playButtons          -< pStatus
>       (bop, pStatus')     <- getBOp msgs          -< (pStatus, pe)
>       tp                  <- unique<<<tempoSlider -< ()
>       (maybeMsgs, isEmpty)<- eventBuffer          -< maybe bop (\x->SetBufferTempo x bop) tp
>       let pStatus'' = if isEmpty then PStopped else pStatus'
>   trackProgress songLength -< (pStatus'',pe, tp)
>   let maybeMsgs' = checkStop bop ~++ maybeMsgs
>   midiOut -< (dev, fmap (map Std) maybeMsgs')
>   returnA -< (maybeMsgs, pStatus'', if isEmpty then ResetAll else getResetDisplay bop)
>   where checkStop bop = if shouldClearBuffer bop then Just (stopAllNotes [0..15]) else Nothing
>         songLength = sum $ map fst msgs

> trackProgress :: DeltaT->UISF (PlayStatus, SEvent PlayEvent, SEvent DeltaT) ()
> trackProgress l = proc (ps,pe,tp) -> do 
>   t     <- getTime -<()
>   t'    <- delay 0 -< t
>   speed <- hold 1  -< tp
>   let delta = t-t'
>   let func = case (ps,pe) of
>                (Playing, Just (PSkip d))->Just (+(delta*speed+d))
>                (Paused,  Just (PSkip d))->Just (+d)
>                (Playing,_)->Just (+delta*speed)
>                (PStopped,_)->Just (const 0)
>                _->Nothing
>   prog <- accum 0 -< func
>   displayTrack -< (truncate prog, truncate l)
>   returnA      -< ()
>   where displayTrack = leftRight $ proc (p, l) -> do 
>           display-< p
>           label "/" -< ()
>           display-< l


playMidArrow takes an array of timed midi messages and creates a MUI that has a 
player UI, including buttons for play/resume/stop/skip-ahead and sliders for the 
amount to skip-ahead as well as for the playback speed. It generates a stream of 
midi message events that are occurring at each time slot.

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

> getResetDisplay :: BufferOperation a->ResetDisplay
> getResetDisplay bop = case bop of
>   ClearBuffer             -> ResetAll
>   SkipAheadInBuffer   _   -> ResetNotes
>   _                       -> NoReset

> tempoSlider :: UISF () Double
> tempoSlider = label "Playback Speed" >>> (leftRight . withCustomDisplay "x" $ mySlider 10 ((1/10),10) 1)

> deltaTSlider :: UISF () DeltaT
> deltaTSlider = label "Skip Ahead" >>> (leftRight . withCustomDisplay "s" $ mySlider 10 ((1/10), 30) 1)

Button controls for playMidArrow

> playButtons :: UISF PlayStatus (SEvent PlayEvent)
> playButtons = proc ps -> do 
>   if ps == PStopped 
>     then fmap (const Play) ^<< edge<<<button "play"-<()
>     else do dt<- deltaTSlider -<()
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
> withCustomDisplay str arrow = proc a -> do 
>   b <- arrow -< a
>   setSize (50,10) displayStr -< show b ++ str
>   returnA                    -< b

A Slider for rationals, essentially a wrapper around hiSlider.

> mySlider :: Int->(Rational, Rational)->Rational->UISF () Double
> mySlider scale (low, high) def = let [low', high', def'] = map (truncate.(*(fromIntegral scale))) [low, high, def]
>                                      intSlider = hiSlider scale (low', high') def'
>                                  in intSlider>>^((/fromIntegral scale).fromIntegral)
