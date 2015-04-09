> module Visualize.Music where
> import Euterpea
> import Euterpea.IO.MUI.MidiWidgets
> import Codec.Midi

> myDiv :: (Integral a)=>a->a->Double
> myDiv x y = fromIntegral x / fromIntegral y

> midiToMsgs :: Midi->[(DeltaT, Message)]
> midiToMsgs mid = let timeD    = timeDiv mid
>                      track    = head . tracks . toSingleTrack $ mid
>                   in convertTrack timeD defaultMSPB track

> convertTrack :: TimeDiv->Int->[(Ticks, Message)]->[(DeltaT, Message)]
> convertTrack _  _  []         = []
> convertTrack td te ((t,m):ts) = let newTempo = case m of
>                                       TempoChange x -> x
>                                       _             -> te        
>                                     delta = getDeltaT td te t
>                                 in (delta, m):convertTrack td newTempo ts

> getDeltaT :: TimeDiv->Int->Ticks->DeltaT
> getDeltaT (TicksPerSecond fps tpf) _    t = t `myDiv` (fps*tpf)
> getDeltaT (TicksPerBeat   tpb)     mspb t = (t * mspb) `myDiv` (tpb*1000000)

Default Microseconds Per Beat

> defaultMSPB = 500000 :: Codec.Midi.Tempo


> getMsgsByChannel :: Channel->[Message]->[Message]
> getMsgsByChannel c [] = []
> getMsgsByChannel c (m:ms) = let ms' = getMsgsByChannel c ms
>                             in if channel m == c then m:ms' else ms'

> getChannelMsgs :: [Message]->[Message]
> getChannelMsgs = filter channelSpecificFilter


> channelSpecificFilter :: Message->Bool
> channelSpecificFilter (NoteOff       _ _ _) = True
> channelSpecificFilter (NoteOn        _ _ _) = True
> channelSpecificFilter (KeyPressure   _ _ _) = True
> channelSpecificFilter (ControlChange _ _ _) = True
> channelSpecificFilter (ProgramChange _ _  ) = True
> channelSpecificFilter (ChannelPressure _ _) = True
> channelSpecificFilter (PitchWheel    _ _  ) = True
> channelSpecificFilter _                     = False
