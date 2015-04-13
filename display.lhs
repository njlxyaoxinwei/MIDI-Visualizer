> {-# LANGUAGE Arrows #-}

> module Visualize.Display where
> import Visualize.Music
> import Euterpea
> import Codec.Midi

> type ChannelDisplayStatus = Maybe Channel -- Nothing: display all

Process an Event of Messages, a wrapper around Visualize.Music.groupMsgs

> groupMsgEvents :: SEvent [Message]->([Message], [[Message]])
> groupMsgEvents Nothing     = ([], replicate 16 [])
> groupMsgEvents (Just msgs) = groupMsgs msgs

ChannelDisplay Handler

> displayArrow :: UISF [[Message]] ()
> displayArrow = proc cs -> do 
>   rec st <- delay Nothing -< st'
>       st'<- do case st of
>                  Nothing -> displayChannels [0..15] -< cs
>                  Just c  -> displaySingleChannel    -< (c, cs!!c)
>   returnA-<()



Display channel information for list of channels

> displayChannels :: [Channel]->UISF [[Message]] ChannelDisplayStatus
> displayChannels []     = arr (const Nothing)
> displayChannels (c:cs) = proc msgs -> do 
>   e <- displayChannel c   -< msgs!!c
>   e'<- displayChannels cs -< msgs
>   case e of 
>     Nothing->returnA-<e'
>     Just _ ->returnA-<Just c


Display Single Channel 

> displaySingleChannel :: UISF (Channel, [Message]) ChannelDisplayStatus
> displaySingleChannel = proc (c, msgs) -> do 
>   displayStr -< "Channel "++show (c+1)
>   notes <- getUpdateArrow [] updateNoteInfo            -< msgs
>   let vs = Just $ map fromIntegral $ plotVelocity notes
>   histogram (makeLayout (Stretchy 300) (Stretchy 300)) -< vs
>   e<-edge<<<button "Back" -< ()
>   case e of 
>     Nothing-> returnA-< Just c
>     Just _ -> returnA-< Nothing


Display row channel information

> displayChannel :: Channel->UISF [Message] (SEvent ())
> displayChannel c = leftRight $ proc msgs -> do 
>   label $ "Channel "++show (c+1) -< ()
>   e<-edge<<<button "Detail"-<()
>   notes <- getUpdateArrow [] updateNoteInfo                       -< msgs
>   vol   <- getUpdateArrow defaultChannelVolume updateChannelVolume  -< msgs
>   inst  <- getUpdateArrow defaultInstrumentName updateInstrumentName -< msgs
>   -- display<<<label "Notes and Velocity:" -< map (\(ap,v)->(pitch ap, v)) notes
>   display<<<label "Intrument: " -< inst
>   display<<<label "Volume: " -< vol
>   let vs = Just $ map fromIntegral $ plotVelocity notes
>   histogram (makeLayout (Fixed 300) (Fixed 35)) -< vs
>   returnA -< e

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


