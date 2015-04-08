> {-# LANGUAGE Arrows #-}

> module Visualize.Play where
> import Euterpea
> import Codec.Midi
> import Euterpea.IO.MUI.MidiWidgets
> import FRP.UISF.AuxFunctions


> playMid :: [(DeltaT, MidiMessage)]->IO ()
> playMid = runMUI defaultMUIParams . playMidArrow

> playMidArrow :: [(DeltaT, MidiMessage)]->UISF () ()
> playMidArrow msgs = proc _ -> do 
>   dev<-selectOutput-<()
>   rec playing <- delay False -< playing'
>       e <- do if playing then edge<<<button "stop"-<()
>                          else edge<<<button "play"-<()
>       (maybeMsgs,isemp)<- eventBuffer <<< getBufferOp msgs -< (e, playing)
>       midiOut-<(dev, maybeMsgs)
>       let playing' = not isemp
>   returnA-<()

> getBufferOp :: [(DeltaT, MidiMessage)]->UISF (SEvent (), Bool) (BufferOperation MidiMessage)
> getBufferOp msgs = proc (e, p) -> do 
>                      case e of 
>                        Nothing -> returnA -< NoBOp
>                        Just x -> do 
>                          if p then returnA -< ClearBuffer
>                          else returnA -< AppendToBuffer msgs



