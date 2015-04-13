> module Visualize.Music where
> import Euterpea
> import Euterpea.IO.MUI.MidiWidgets
> import Codec.Midi
> import Data.List

> type NoteInfo = (Key, Velocity)
> type UpdateFunc a = a->[Message]->a
> type ChannelVolume = (Int, Int)  --Correspond to Controller 7 and 11 


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


Default Values

1. Default Microseconds Per Beat

> defaultMSPB = 500000 :: Codec.Midi.Tempo

2. Default Channel Volume/Expression (CC#7 and CC#11)

> defaultChannelVolume = (100,100) :: ChannelVolume

3. Default InstrumentName 

> defaultInstrumentName = AcousticGrandPiano

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



> getUpdateFunc :: (a->Message->a)->UpdateFunc a
> getUpdateFunc func original = foldl func original

Update (Key, Velocity) according to a new set of messages

> updateNoteInfo :: UpdateFunc [NoteInfo]
> updateNoteInfo = getUpdateFunc updateOneNote where
>   updateOneNote nis (NoteOff _ k v)         = deleteBy (\(k1,_) (k2,_)->k1==k2) (k,v) nis
>   updateOneNote nis (NoteOn  _ k 0)         = updateOneNote nis (NoteOff 0 k 0)
>   updateOneNote nis (NoteOn  _ k v)         = let nis' = updateOneNote nis (NoteOff 0 k v)
>                                               in insert (k,v) nis'
>   updateOneNote nis (ControlChange _ 123 0) = []
>   updateOneNote nis _                       = nis

Update InstrumentName

> updateInstrumentName :: UpdateFunc InstrumentName
> updateInstrumentName = getUpdateFunc updateInst where
>   updateInst n (ProgramChange _ x) = toEnum x
>   updateInst n _                   = n 

Update BPM

> updateMSPB :: UpdateFunc Codec.Midi.Tempo
> updateMSPB = getUpdateFunc update where
>   update t (TempoChange x) = x
>   update t _               = t

A Velocity Function from NoteInfo on [0..127]

> plotVelocity :: [NoteInfo]->[Velocity]
> plotVelocity = plotVelocity' 0 where
>   plotVelocity' 128 _             = []
>   plotVelocity' k []              = 0:plotVelocity' (k+1) []
>   plotVelocity' k nis@((k',v):ns) = if k==k' then (v:plotVelocity' (k+1) ns)
>                                              else (0:plotVelocity' (k+1) nis)

> updateChannelVolume :: UpdateFunc ChannelVolume
> updateChannelVolume = getUpdateFunc update where
>   update (v7,v11) (ControlChange _ 7  x) = (x, v11)
>   update (v7,v11) (ControlChange _ 11 x) = (v7,  x)
>   update (v7,v11) _                      = (v7,v11)
