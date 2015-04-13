> {-# LANGUAGE Arrows #-}

> module Visualize.Display where
> import Visualize.Music
> import Euterpea
> import Codec.Midi

> type ChannelDisplayStatus = Maybe Channel -- Nothing: display all
> type ChannelInfo = ([NoteInfo], InstrumentName, ChannelVolume)

Process an Event of Messages, a wrapper around Visualize.Music.groupMsgs

> groupMsgEvents :: SEvent [Message]->([Message], [[Message]])
> groupMsgEvents Nothing     = ([], replicate 16 [])
> groupMsgEvents (Just msgs) = groupMsgs msgs

ChannelDisplay Handler

> displayArrow :: UISF [[Message]] ()
> displayArrow = proc cs -> do 
>   infos <- getAllChannelInfo [0..15] -< cs
>   rec st <- delay Nothing -< st'
>       st'<- do case st of
>                  Nothing -> displayChannels [0..15] -< infos
>                  Just c  -> displaySingleChannel    -< (c, infos!!c)
>   returnA-<()


Process Channel Specific Messages

> getAllChannelInfo :: [Channel]-> UISF [[Message]] [ChannelInfo]
> getAllChannelInfo []     = arr (const [])
> getAllChannelInfo (c:cs) = proc msgs -> do 
>    info <- getChannelInfo -< msgs!!c
>    infos<- getAllChannelInfo cs-< msgs
>    returnA-< info:infos

> getChannelInfo :: UISF [Message] ChannelInfo
> getChannelInfo = proc msgs -> do 
>   notes <- getUpdateArrow []                    updateNoteInfo       -< msgs
>   inst  <- getUpdateArrow defaultInstrumentName updateInstrumentName -< msgs
>   vol   <- getUpdateArrow defaultChannelVolume  updateChannelVolume  -< msgs
>   returnA -< (notes, inst, vol)


Display channel information for list of channels

> displayChannels :: [Channel]->UISF [ChannelInfo] ChannelDisplayStatus
> displayChannels []     = arr (const Nothing)
> displayChannels (c:cs) = proc infos -> do 
>   e <- displayChannel c   -< infos!!c
>   e'<- displayChannels cs -< infos
>   case e of 
>     Nothing->returnA-<e'
>     Just _ ->returnA-<Just c


Display Single Channel 

> displaySingleChannel :: UISF (Channel, ChannelInfo) ChannelDisplayStatus
> displaySingleChannel = proc (c, (notes, inst, vol)) -> do 
>   displayStr -< "Channel "++show (c+1)
>   display -< notes
>   display -< inst
>   display -< vol
>   let vs = Just $ map fromIntegral $ plotVelocity notes
>   histogram (makeLayout (Stretchy 300) (Stretchy 300)) -< vs
>   e<-edge<<<button "Back" -< ()
>   case e of 
>     Nothing-> returnA-< Just c
>     Just _ -> returnA-< Nothing


Display row channel information

> displayChannel :: Channel->UISF ChannelInfo (SEvent ())
> displayChannel c = leftRight $ proc (notes, inst, vol) -> do 
>   label $ "Channel "++show (c+1) -< ()
>   e<-edge<<<button "Detail"-<()
>   display<<<label "Intrument: " -< inst
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


