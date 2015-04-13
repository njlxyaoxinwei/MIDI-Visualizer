> module Visualize.Music where
> import Euterpea
> import Euterpea.IO.MUI.MidiWidgets
> import Codec.Midi
> import Data.List

A wrapper for dividing integers

> myDiv :: (Integral a)=>a->a->Double
> myDiv x y = fromIntegral x / fromIntegral y

Convert Codec.Midi.Midi to an array of timed messages, compatible with 
AppendToBuffer BufferOperation.

> midiToMsgs :: Midi->[(DeltaT, Message)]
> midiToMsgs mid = let timeD    = timeDiv mid
>                      track    = head . tracks . toSingleTrack $ mid
>                   in convertTrack timeD defaultMSPB track

Given the current TimeDiv and Microseconds Per Beat, and a midi Track, convert
Midi Ticks to Seconds (DeltaT).

> convertTrack :: TimeDiv->Int->[(Ticks, Message)]->[(DeltaT, Message)]
> convertTrack _  _  []         = []
> convertTrack td te ((t,m):ts) = let newTempo = case m of
>                                       TempoChange x -> x
>                                       _             -> te        
>                                     delta = getDeltaT td te t
>                                 in (delta, m):convertTrack td newTempo ts

Convert Ticks to seconds according to the given TimeDiv and Microseconds Per 
Beat

> getDeltaT :: TimeDiv->Int->Ticks->DeltaT
> getDeltaT (TicksPerSecond fps tpf) _    t = t `myDiv` (fps*tpf)
> getDeltaT (TicksPerBeat   tpb)     mspb t = (t * mspb) `myDiv` (tpb*1000000)

Default Microseconds Per Beat

> defaultMSPB = 500000 :: Codec.Midi.Tempo

Divide the array of messages into System-Wide messages and Channel-Specific 
ones, the latter further divided into a list of 16 lists, each corresponding to
a channel.

> groupMsgs :: [Message]->([Message], [[Message]])
> groupMsgs msgs = let (chsMsgs,sysMsgs) = partition channelSpecificFilter msgs
>                  in  (sysMsgs, map (getMsgsByChannel chsMsgs) [0..15])

> getMsgsByChannel :: [Message]->Channel->[Message]
> getMsgsByChannel []     _ = []
> getMsgsByChannel (m:ms) c = let ms' = getMsgsByChannel ms c
>                             in if channel m == c then m:ms' else ms'

> channelSpecificFilter :: Message->Bool
> channelSpecificFilter (NoteOff       _ _ _) = True
> channelSpecificFilter (NoteOn        _ _ _) = True
> channelSpecificFilter (KeyPressure   _ _ _) = True
> channelSpecificFilter (ControlChange _ _ _) = True
> channelSpecificFilter (ProgramChange _ _  ) = True
> channelSpecificFilter (ChannelPressure _ _) = True
> channelSpecificFilter (PitchWheel    _ _  ) = True
> channelSpecificFilter _                     = False
