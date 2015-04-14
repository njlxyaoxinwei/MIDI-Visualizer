> {-# LANGUAGE Arrows #-}

> module Visualize.Display where
> import Visualize.Play
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

> displayArrow :: UISF ([[Message]], ResetDisplay) ()
> displayArrow = proc (cs,rd) -> do 
>   infos <- getAllChannelInfo [0..15] -< (cs,rd)
>   rec st <- delay Nothing -< st'
>       st'<- do case st of
>                  Nothing -> displayChannels [0..15] -< infos
>                  Just c  -> displaySingleChannel    -< (c, infos!!c)
>   returnA-<()


Process Channel Specific Messages

> getAllChannelInfo :: [Channel]-> UISF ([[Message]], ResetDisplay) [ChannelInfo]
> getAllChannelInfo []     = arr (const [])
> getAllChannelInfo (c:cs) = proc (msgs,rd) -> do 
>    info <- getChannelInfo -< (msgs!!c,rd)
>    infos<- getAllChannelInfo cs-< (msgs,rd)
>    returnA-< info:infos

> getChannelInfo :: UISF ([Message], ResetDisplay) ChannelInfo
> getChannelInfo = proc (msgs,rd) -> do 
>   let (reNote, reRest) = case rd of
>                            ResetAll  ->(True,   True)
>                            ResetNotes->(True,  False)
>                            _         ->(False, False)
>   notes <- getUpdateArrow []                    updateNoteInfo       -< (msgs,reNote)
>   inst  <- getUpdateArrow defaultInstrumentName updateInstrumentName -< (msgs,reRest)
>   vol   <- getUpdateArrow defaultChannelVolume  updateChannelVolume  -< (msgs,reRest)
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
>   let vs = Just $ map fromIntegral $ 128:plotVelocity notes
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
>   let vs = Just $ map fromIntegral $ 128:plotVelocity notes
>   histogram (makeLayout (Fixed 300) (Fixed 35)) -< vs
>   returnA -< e

Display System information

> displaySys :: UISF ([Message],ResetDisplay) ()
> displaySys = leftRight $ proc (msgs,rd) -> do
>   tempo <- getUpdateArrow defaultMSPB updateMSPB -< (msgs, rd==ResetAll)
>   display <<<label "BPM: " -< round $ 60000000 / fromIntegral tempo


> getUpdateArrow :: a->UpdateFunc a->UISF ([Message], Bool) a
> getUpdateArrow def update = proc (msgs,flag) -> do 
>   rec oldVal <- delay def -< newVal
>       let newVal = if flag then def else update oldVal msgs
>   returnA -< newVal

================================================================================
For debugging purposes

Display the messages event

> displayMessages :: UISF [Message] ()
> displayMessages = proc msgs -> do 
>   let event = if null msgs then Nothing else Just msgs
>   display <<< hold [] -< event


