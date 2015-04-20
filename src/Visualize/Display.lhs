> {-# LANGUAGE Arrows #-}

> module Visualize.Display (
>   displayArrow, displaySys, ResetDisplay(..),
>   getAllChannelInfo, getChannelInfo,
>   displayChannels,
>   displaySingleChannel, displaySingleDrumChannel, displayDrumHist
> ) where
> import Visualize.Music
> import HistogramUpdate
> import Euterpea hiding (Tempo, a, b, c, cs, e)
> import Codec.Midi (Channel)

> type ChannelDisplayStatus = Maybe Channel -- Nothing: display all
> data ResetDisplay = NoReset | ResetNotes | ResetAll deriving (Show, Eq)


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
> displaySingleChannel = title "Channel Detail" $ proc (c, (notes, inst, vol@(v7,v11))) -> do 
>   e<-backButton-<c
>   leftRight $ display <<< label "Channel Volume (0-127): "            -< v7
>   leftRight $ display <<< label "Current Keys and Velocity (0-127): " -< notes
>   (| leftRight (do display <<< label "Current Instrument: "            -< inst
>                    display <<< label "Instrument Expression (0-127): " -< v11)|)
>   let vs = Just $ plotVolume notes vol
>   histogram' (makeLayout (Stretchy 300) (Stretchy 300)) -< vs
>   case e of 
>     Nothing-> returnA-< Just c
>     Just _ -> returnA-< Nothing

> displaySingleDrumChannel :: UISF ChannelInfo ChannelDisplayStatus
> displaySingleDrumChannel = title "Percussion Channel Detail" $ proc (notes, _, vol@(v7,v11)) -> do 
>   e<-backButton-<9
>   (| leftRight (do display <<< label "Channel Volume (0-127): "             -< v7
>                    display <<< label "Instrument Expression (0-127): "      -< v11)|)
>   leftRight $ display <<< label "Current Sound and Velocity (0-127): " -< map (\(k,v)->(getPerc k, v)) $ filter (\(k,_)->k>=35 && k<=81) notes
>   let vs = toPercussionPlot . plotVolume notes $ vol
>   displayDrumHist -< vs
>   case e of
>     Nothing -> returnA -< Just 9
>     Just _  -> returnA -< Nothing

> displayDrumHist :: UISF [(Double, String)] ()
> displayDrumHist = let drumLayout = makeLayout (Stretchy 300) (Stretchy 75)
>                   in proc vs -> do 
>   let (r1,r2,r3,r4) = divide vs
>   histogramWithScale' drumLayout -< Just r1
>   histogramWithScale' drumLayout -< Just r2
>   histogramWithScale' drumLayout -< Just r3
>   histogramWithScale' drumLayout -< Just r4
>   where divide vs = let l = length vs `div` 4
>                     in (take l vs, take l . drop l $ vs, 
>                         take l . drop (2*l) $ vs, 
>                         drop (3*l) vs)

> backButton :: UISF Channel (SEvent ())
> backButton = leftRight $ proc c -> do 
>   e<-setSize (70,20) $ edge<<<button "Back" -< ()
>   displayStr -< "Channel " ++ show (c+1)
>   returnA    -< e

Display row channel information

> displayChannel :: Channel->UISF ChannelInfo (SEvent ())
> displayChannel c = title ("Channel "++show (c+1)) . leftRight $ proc (notes, inst, vol) -> do 
>   e<-edge<<<setSize (70,20) (button "Detail")-<()
>   setSize (170,20) display -< inst
>   let vs = Just $ plotVolume notes vol
>   histogram' (makeLayout (Stretchy 200) (Stretchy 20)) -< vs
>   returnA -< e


> displayDrumChannel :: UISF ChannelInfo (SEvent ())
> displayDrumChannel = title "Channel 10 (Percussion)" . leftRight $ proc (notes, _ , vol) -> do
>   e<-edge<<<setSize (70,20) (button "Detail") -< ()
>   setSize (170,20) display -< Percussion
>   let vs = Just $ plotVolume notes vol
>   histogram' (makeLayout (Stretchy 200) (Stretchy 20)) -< vs
>   returnA -< e


Display System information

> displaySys :: UISF ([Message],Bool) ()
> displaySys = title "System Info" $ proc (msgs,flag) -> do
>   (t, l, (p, m), (a,b),x) <- getUpdateArrow ("","",defaultKeySig, defaultTimeSig,defaultMSPB) updateSystemInfo -< (msgs, flag)
>   (| leftRight (do display <<< label "BPM: "            -< round $ 60000000 / fromIntegral x 
>                    display <<< label "Time Signature: " -< a
>                    display <<< label "/"                -< b)|)
>   leftRight $ displayStr <<< (\(p,m)->show' p ++ " " ++ show m) ^<< label "Key Signature: " -< (p,m)
>   leftRight $ displayStr <<< label "Text: "                                                 -< t
>   leftRight $ displayStr <<< label "Lyrics: "                                               -< l
>   where show' p = case p of
>                     Af->"Ab"; Bf->"Bb"; Cf->"Cb"; Df->"Db"; Ef->"Eb"; Gf->"Gb"
>                     As->"A#"; Cs->"C#"; Ds->"D#"; Fs->"F#"; Gs->"Gs"; _ ->show p


> getUpdateArrow :: a->UpdateFunc a->UISF ([Message], Bool) a
> getUpdateArrow def update = proc (msgs,flag) -> do 
>   rec oldVal <- delay def -< newVal
>       let newVal = if flag then def else update oldVal msgs
>   returnA -< newVal

================================================================================
For debugging purposes

Display the messages event

--> displayMessages :: UISF [Message] ()
--> displayMessages = proc msgs -> do 
-->   let event = if null msgs then Nothing else Just msgs
-->   display <<< hold [] -< event


