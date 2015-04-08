> {-# LANGUAGE Arrows #-}

> module Play where
> import Euterpea
> import Euterpea.IO.MUI.MidiWidgets
> import Codec.Midi
> import FRP.UISF.AuxFunctions


> playMid :: Midi->IO ()
> playMid = runMUI defaultMUIParams . playMidArrow

> playMidArrow :: Midi->UISF () ()
> playMidArrow mid = proc _ -> do 
>   dev<-selectOutput-<()
>   e<-edge<<<button "play"-<()
>   (maybeMsgs,_)<-eventBuffer <<< getBufferOp mid -< e
>   midiOut-<(dev, maybeMsgs)
>   returnA-<()

> getBufferOp :: Midi->UISF (SEvent ()) (BufferOperation MidiMessage)
> getBufferOp mid = proc e -> do 
>   let midiEvent = fmap (const $ midiToBO mid) e
>   case midiEvent of 
>     Nothing -> returnA-<NoBOp
>     Just bo -> returnA-<bo

> midiToBO :: Midi->BufferOperation MidiMessage
> midiToBO mid = let (m, _, _) = fromMidi mid
>                in musicToBO False [] m


