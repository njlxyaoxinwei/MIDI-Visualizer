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
>   displayChannel c   -< msgs!!c
>   displayChannels cs -< msgs

Display channel information for one channel

> displayChannel :: Channel->UISF (SEvent [Message]) ()
> displayChannel c = leftRight $ label ("Channel" ++ show (c+1)) >>> proc msgs -> do 
>   notes <- getUpdateArrow [] updateNoteInfo                       -< msgs
>   inst  <- getUpdateArrow AcousticGrandPiano updateInstrumentName -< msgs
>   -- display<<<label "Notes and Velocity:" -< map (\(ap,v)->(pitch ap, v)) notes
>   display<<<label "Intrument: " -< inst
>   let vs = Just $ map fromIntegral $ plotVelocity notes
>   histogram (makeLayout (Fixed 300) (Fixed 35)) -< vs

Display System information

> displaySys :: UISF (SEvent [Message]) ()
> displaySys = leftRight $ proc msgs -> do
>   tempo <- getUpdateArrow defaultMSPB updateMSPB -< msgs
>   display <<<label "BPM: " -< round $ 60000000 / fromIntegral tempo


> getUpdateArrow :: a->UpdateFunc a->UISF (SEvent [Message]) a
> getUpdateArrow def func = proc msgs -> do 
>   rec oldVal <- delay def -< newVal
>       let newVal = maybe oldVal (func oldVal) msgs
>   returnA -< newVal

================================================================================
For debugging purposes

Display the messages event

> displayMessages :: UISF (SEvent [Message]) ()
> displayMessages = proc msgs -> do 
>   display <<< hold [] -< msgs


