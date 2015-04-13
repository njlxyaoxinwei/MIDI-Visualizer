> {-# LANGUAGE Arrows #-}

> module Visualize.Display where
> import Visualize.Music
> import Euterpea
> import Codec.Midi

Process an Event of Messages, a wrapper around Visualize.Music.groupMsgs

> groupMsgEvents :: SEvent [Message]->([Message], [[Message]])
> groupMsgEvents Nothing     = ([], replicate 16 [])
> groupMsgEvents (Just msgs) = groupMsgs msgs

Display channel information for list of channels

> displayChannels :: [Channel]->UISF [[Message]] ()
> displayChannels []     = arr (const ())
> displayChannels (c:cs) = proc msgs -> do 
>   displayChannel c   -< msgs!!c
>   displayChannels cs -< msgs

Display channel information for one channel

> displayChannel :: Channel->UISF [Message] ()
> displayChannel c = leftRight $ label ("Channel" ++ show (c+1)) >>> proc msgs -> do 
>   notes <- getUpdateArrow [] updateNoteInfo                       -< msgs
>   vol   <- getUpdateArrow defaultChannelVolume updateChannelVolume  -< msgs
>   inst  <- getUpdateArrow defaultInstrumentName updateInstrumentName -< msgs
>   -- display<<<label "Notes and Velocity:" -< map (\(ap,v)->(pitch ap, v)) notes
>   display<<<label "Intrument: " -< inst
>   display<<<label "Volume: " -< vol
>   let vs = Just $ map fromIntegral $ plotVelocity notes
>   histogram (makeLayout (Fixed 300) (Fixed 35)) -< vs

Display System information

> displaySys :: UISF [Message] ()
> displaySys = leftRight $ proc msgs -> do
>   tempo <- getUpdateArrow defaultMSPB updateMSPB -< msgs
>   display <<<label "BPM: " -< round $ 60000000 / fromIntegral tempo


> getUpdateArrow :: a->UpdateFunc a->UISF [Message] a
> getUpdateArrow def update = proc msgs -> do 
>   rec oldVal <- delay def -< newVal
>       let newVal = update oldVal msgs
>   returnA -< newVal

================================================================================
For debugging purposes

Display the messages event

> displayMessages :: UISF [Message] ()
> displayMessages = proc msgs -> do 
>   let event = if null msgs then Nothing else Just msgs
>   display <<< hold [] -< event


