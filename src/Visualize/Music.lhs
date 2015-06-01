> -- Module        : Visualize.Music
> -- Copyright     : (c) 2015 Xinwei Yao
> -- Date Modified : 6/1/2015
> --
> -- Music Module of Visualize, providing manipulation of MIDI messages

> module Visualize.Music (
>   midiToMsgs, groupMsgs,
>   NoteInfo, ChannelVolume, SystemInfo, ChannelInfo,
>   defaultMSPB, defaultChannelVolume, defaultTimeSig,
>   defaultKeySig, defaultInstrumentName,
>   UpdateFunc,
>   updateNoteInfo, updateChannelVolume, 
>   updateSystemInfo, updateInstrumentName,
>   plotVolume, toPercussionPlot, getPerc,
>   Message, DeltaT, InstrumentName(..), Mode(..), PercussionSound, PitchClass(..)
> ) where
> import Euterpea (PitchClass(..), InstrumentName(..), Mode(..), PercussionSound)
> import Codec.Midi 
> import FRP.UISF.AuxFunctions (DeltaT)
> import Data.List (partition, deleteBy, insert)


An UpdateFunc is a function that updates some information with a list of 
Midi Messages. They are used in the Display Module to update the display.

> type UpdateFunc a = a->[Message]->a

NoteInfo, ChannelVolume, SystemInfo, ChannelInfo, KeySig, TimeSig are all 
information that visualize keeps track of at any given instant.

> type NoteInfo = (Key, Velocity)
> type ChannelVolume = (Int, Int)  --Correspond to Controller 7 (Channel Volume) and 11 (Expression)
> type SystemInfo = (String, String, KeySig, TimeSig, Tempo)  --Text, Lyric, ...
> type ChannelInfo = ([NoteInfo], InstrumentName, ChannelVolume)
> type KeySig = (PitchClass, Mode)
> type TimeSig = (Int, Int)


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

> defaultMSPB :: Tempo
> defaultMSPB = 500000

2. Default Channel Volume/Expression (CC#7 and CC#11)

> defaultChannelVolume :: ChannelVolume
> defaultChannelVolume = (100,100)

3. Default InstrumentName 

> defaultInstrumentName :: InstrumentName
> defaultInstrumentName = AcousticGrandPiano

4. Default TimeSignature

> defaultTimeSig :: TimeSig
> defaultTimeSig = (4,4)

5. Default Key Signature

> defaultKeySig :: KeySig
> defaultKeySig = (C, Major)

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


Given a function that updates the a value according to one message, get a 
function that takes a list of messages and updates the a value by going through 
the messages one by one.

> getUpdateFunc :: (a->Message->a)->UpdateFunc a
> getUpdateFunc func original = foldl func original

UpdateFuncs for various information:

1. Update current list of (Key, Velocity) according to a new set of messages

> updateNoteInfo :: UpdateFunc [NoteInfo]
> updateNoteInfo = getUpdateFunc updateOneNote where
>   updateOneNote nis (NoteOff _ k v)         = deleteBy (\(k1,_) (k2,_)->k1==k2) (k,v) nis
>   updateOneNote nis (NoteOn  _ k 0)         = updateOneNote nis (NoteOff 0 k 0)
>   updateOneNote nis (NoteOn  _ k v)         = let nis' = updateOneNote nis (NoteOff 0 k v)
>                                               in insert (k,v) nis'
>   updateOneNote _   (ControlChange _ 123 0) = []
>   updateOneNote nis _                       = nis

2. Update InstrumentName according to a new set of messages

> updateInstrumentName :: UpdateFunc InstrumentName
> updateInstrumentName = getUpdateFunc updateInst where
>   updateInst _ (ProgramChange _ x) = toEnum x
>   updateInst n _                   = n 

3. Update ChannelVolume

> updateChannelVolume :: UpdateFunc ChannelVolume
> updateChannelVolume = getUpdateFunc update where
>   update (_ ,v11) (ControlChange _ 7  x) = (x, v11)
>   update (v7,_  ) (ControlChange _ 11 x) = (v7,  x)
>   update (v7,v11) _                      = (v7,v11)

4. Update SystemInfo, i.e. Text, Lyrics, TimeSig, KeySig and Tempo

> updateSystemInfo :: UpdateFunc SystemInfo
> updateSystemInfo = getUpdateFunc update where
>   update (_,l,(p,m),(a,b),x ) (Text   s)              = (s, l, (p,m), (a,b), x)
>   update (t,_,(p,m),(a,b),x ) (Lyrics s)              = (t, s, (p,m), (a,b), x)
>   update (t,l,(p,m),_    ,x ) (TimeSignature a b _ _) = (t, l, (p,m), (getTimeSig a b), x)
>   update (t,l,_    ,(a,b),x ) (KeySignature p m)      = (t, l, (getKeySig p m), (a,b), x)
>   update (t,l,(p,m),(a,b),_ ) (TempoChange x)         = (t, l, (p,m), (a,b), x)
>   update (t,l,(p,m),(a,b),x ) _                       = (t, l, (p,m), (a,b), x)                  




A Velocity Function from NoteInfo on [0..127], ranging from 0 to 1.

> plotVolume :: [NoteInfo]->ChannelVolume->[Double]
> plotVolume nis (v7,v11) = plot' 0 nis where
>   e = (v7*v11) `myDiv` (127*127*127)
>   plot' 128 _             = []
>   plot' k []              = 0:plot' (k+1) []
>   plot' k nis'@((k',v):ns) = if k==k' then ((fromIntegral v*e):plot' (k+1) ns)
>                                       else (0:plot' (k+1) nis')

Get PercussionSound from key, assuming the key is a valid PercussionSound.

> getPerc :: Key->PercussionSound
> getPerc k = toEnum (k-35)

Take a histogram of key volumes, get the subset corresponding to 
PercussionSounds, and attach the names of the sounds to the histogram.

> toPercussionPlot :: [Double]->[(Double, String)]
> toPercussionPlot vs = let slice = take 47 . drop 35 $ vs
>                           ps    = map toEnum [0..46] :: [PercussionSound]
>                       in  zip slice (map show ps)

Parser for TimeSignature

> getTimeSig :: Int->Int->TimeSig
> getTimeSig a b = (a, 2^b)

Parser for KeySignature

> getKeySig :: Int->Int->KeySig
> getKeySig a 0 = (majorKeyList!!(a+7), Major)
> getKeySig a 1 = (minorKeyList!!(a+7), Minor)
> getKeySig a x = error $ "Wrong Input" ++ show a ++ " " ++ show x


The helper list for determining KeySignatures: 
majorKeyList starts from the major key with 7 flats and goes up to the major key
with 7 sharps; minorKeyList does the same thing for minor keys.

> majorKeyList, minorKeyList :: [PitchClass]
> majorKeyList = [Cf, Gf, Df, Af, Ef, Bf, F, C, G, D, A, E, B, Fs, Cs]
> minorKeyList = [Af, Ef, Bf, F, C, G, D, A, E, B, Fs, Cs, Gs, Ds, As]
