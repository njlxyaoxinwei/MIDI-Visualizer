> {-# LANGUAGE Arrows #-}

> module Visualize.Display where
> import Visualize.Music
> import Euterpea
> import Codec.Midi

Process an Event of Messages, a wrapper around Visualize.Music.groupMsgs

> groupMsgEvents :: SEvent [Message]->(SEvent [Message], [SEvent [Message]])
> groupMsgEvents Nothing     = let (_,cs) = groupMsgs []
>                              in (Nothing, map (const Nothing) cs)
> groupMsgEvents (Just msgs) = let (ss, cs) = groupMsgs msgs
>                              in (helper ss, map helper cs) where
>   helper [] = Nothing
>   helper xs = Just xs

Display channel information for list of channels

> displayChannels :: [Channel]->UISF [SEvent [Message]] ()
> displayChannels []     = arr (const ())
> displayChannels (c:cs) = proc msgs -> do 
>   displayChannel     -< msgs!!c
>   displayChannels cs -< msgs

Display channel information for one channel

> displayChannel :: UISF (SEvent [Message]) ()
> displayChannel = leftRight $ proc msgs -> do 
>   notes <- getUpdateArrow [] updateNoteInfo                       -< msgs
>   inst  <- getUpdateArrow AcousticGrandPiano updateInstrumentName -< msgs
>   display<<<label "Notes and Volume:" -< map (\(ap,v)->(pitch ap, v)) notes
>   display<<<label "with" -< inst


> getUpdateArrow :: a->UpdateFunc a->UISF (SEvent [Message]) a
> getUpdateArrow def func = proc msgs -> do 
>   rec oldVal <- delay def -< newVal
>       let newVal = maybe oldVal (func oldVal) msgs
>   returnA -< newVal

--Update note information

--> updateNotes :: UISF (SEvent [Message]) [NoteInfo]
--> updateNotes = proc msgs -> do 
-->   rec notes <- delay [] -< notes'
-->       let notes' = maybe notes (updateNoteInfo notes) msgs
-->   returnA -< notes'

--Update Instrument Information

--> updateInstrument :: UISF (SEvent [Message]) InstrumentName
--> updateInstrument = proc msgs -> do 
-->   rec inst <- delay 0 -< inst'
-->       let inst' = inst
-->   returnA -< toEnum inst'




================================================================================
For debugging purposes

Display the messages event

> displayMessages :: UISF (SEvent [Message]) ()
> displayMessages = proc msgs -> do 
>   display <<< hold [] -< msgs


