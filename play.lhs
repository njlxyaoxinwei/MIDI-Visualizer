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
>   rec playing <- delay False -< playing'
>       e<- if playing then edge<<<button "stop"-<()
>                      else edge<<<button "play"-<()
>       (maybeMsgs,isemp)<- eventBuffer <<< getBufferOp mid -< (e, playing)
>       midiOut-<(dev, maybeMsgs)
>       let playing' = not isemp
>   returnA-<()

> getBufferOp :: Midi->UISF (SEvent (), Bool) (BufferOperation MidiMessage)
> getBufferOp mid = proc (e, p) -> do 
>   case e of 
>     Nothing -> returnA -< NoBOp
>     Just x -> do 
>       if p then returnA -< ClearBuffer
>            else returnA -< midiToBO mid

> midiToBO :: Midi->BufferOperation MidiMessage
> midiToBO mid = let (m, _, _) = fromMidi mid
>                in musicToBO False [] m


