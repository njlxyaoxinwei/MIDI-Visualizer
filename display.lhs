> {-# LANGUAGE Arrows #-}

> module Visualize.Display where
> import Visualize.Play
> import Visualize.Music
> import HistogramUpdate
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
>                  Nothing -> twoColumns -< infos
>                  Just 9  -> displaySingleDrumChannel-< infos!!9
>                  Just c  -> displaySingleChannel    -< (c, infos!!c)
>   returnA-<()
>   where twoColumns = leftRight $ proc infos -> do 
>           ds1<-displayChannels [0..7] -< infos
>           ds2<-displayChannels [8..15]-< infos
>           returnA -< maybe ds2 (const ds1) ds1


Process Channel Specific Messages

> getAllChannelInfo :: [Channel]-> UISF ([[Message]], ResetDisplay) [ChannelInfo]
> getAllChannelInfo []     = arr (const [])
> getAllChannelInfo (c:cs) = proc (msgs,rd) -> do 
>    info <- getChannelInfo -< (msgs!!c,rd)
>    infos<- getAllChannelInfo cs-< (msgs,rd)
>    returnA-< info:infos

> getChannelInfo :: UISF ([Message], ResetDisplay) ChannelInfo
> getChannelInfo = proc (msgs,rd) -> do 
>   let (reNotes, reChannel) = parseResetDisplay rd
>   notes <- getUpdateArrow []                    updateNoteInfo       -< (msgs,reNotes)
>   inst  <- getUpdateArrow defaultInstrumentName updateInstrumentName -< (msgs,reChannel)
>   vol   <- getUpdateArrow defaultChannelVolume  updateChannelVolume  -< (msgs,reChannel)
>   returnA -< (notes, inst, vol)
>   where parseResetDisplay rd = case rd of
>                                  ResetAll  -> (True,  True)
>                                  ResetNotes-> (True, False)
>                                  _         -> (False,False)

Display channel information for list of channels

> displayChannels :: [Channel]->UISF [ChannelInfo] ChannelDisplayStatus
> displayChannels []     = arr (const Nothing)
> displayChannels (c:cs) = topDown $ proc infos -> do 
>   e <- do if c==9 then displayDrumChannel -< infos!! c 
>                   else displayChannel c -< infos!!c
>   e'<- displayChannels cs -< infos
>   case e of 
>     Nothing->returnA-<e'
>     Just _ ->returnA-<Just c


Display Single Channel 

> displaySingleChannel :: UISF (Channel, ChannelInfo) ChannelDisplayStatus
> displaySingleChannel = title "Channel Detail" $ proc (c, (notes, inst, vol)) -> do 
>   e<-edge<<<button "Back" -< ()
>   displayStr -< "Channel "++show (c+1)
>   display -< notes
>   display -< inst
>   display -< vol
>   let vs = Just $ plotVolume notes vol
>   histogram' (makeLayout (Stretchy 300) (Stretchy 300)) -< vs
>   case e of 
>     Nothing-> returnA-< Just c
>     Just _ -> returnA-< Nothing

> displaySingleDrumChannel :: UISF ChannelInfo ChannelDisplayStatus
> displaySingleDrumChannel = title "Percussion Channel Detail" $ proc (notes, _, vol) -> do 
>   e<-edge<<<button "Back" -<()
>   display -< notes
>   display -< vol
>   let vs = plotPercussion . plotVolume notes $ vol
>   title "Percussion Instruments" displayDrumHist -< vs
>   case e of
>     Nothing -> returnA -< Just 9
>     Just _  -> returnA -< Nothing

> displayDrumHist :: UISF [(Double, String)] ()
> displayDrumHist = let drumLayout = makeLayout (Fixed 250) (Stretchy 75)
>                   in proc vs -> do 
>   let (r1,r2,r3,r4) = divide vs
>   histogramWithScale' drumLayout -< Just (stub r1)
>   histogramWithScale' drumLayout -< Just (stub r2)
>   histogramWithScale' drumLayout -< Just (stub r3)
>   histogramWithScale' drumLayout -< Just (stub r4)
>   where divide vs = let l = length vs `div` 4
>                     in (take l vs, take l . drop l $ vs, 
>                         take l . drop (2*l) $ vs, 
>                         drop (3*l) vs)
>         stub vs = (0, ""):vs ++ [(0, "")]


Display row channel information

> displayChannel :: Channel->UISF ChannelInfo (SEvent ())
> displayChannel c = title ("Channel "++show (c+1)) . leftRight $ proc (notes, inst, vol) -> do 
>   e<-edge<<<setSize (70,20) (button "Detail")-<()
>   setSize (170,20) display -< inst
>   let vs = Just $ plotVolume notes vol
>   histogram' (makeLayout (Stretchy 300) (Stretchy 25)) -< vs
>   returnA -< e


> displayDrumChannel :: UISF ChannelInfo (SEvent ())
> displayDrumChannel = title "Channel 10 (Percussion)" . leftRight $ proc (notes, inst, vol) -> do
>   e<-edge<<<setSize (70,20) (button "Detail") -< ()
>   setSize (170,20) display -< Percussion
>   let vs = Just $ plotVolume notes vol
>   histogram' (makeLayout (Stretchy 300) (Stretchy 25)) -< vs
>   returnA -< e


Display System information

> displaySys :: UISF ([Message],Bool) ()
> displaySys = title "System Info" $ proc (msgs,flag) -> do
>   tempo <- getUpdateArrow defaultMSPB updateMSPB -< (msgs, flag)
>   leftRight (display <<<label "BPM: ") -< round $ 60000000 / fromIntegral tempo


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


