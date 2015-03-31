> {-# LANGUAGE Arrows #-}

> module TestProject where
> import Euterpea
> import Euterpea.Examples.MUI
> import Euterpea.IO.MUI.MidiWidgets
> import Codec.Midi
> import FRP.UISF.AuxFunctions


> getPlayMUI :: BufferOperation MidiMessage->UISF () ()
> getPlayMUI buf = proc _ -> do 
>   dev<-selectOutput-<()
>   e<-edge<<<button "play"-<()
>   e2<-unique<<<stickyButton "pause"-<()
>   t<-unique<<<withDisplay (hSlider (0.1,2.0) 1.0) -< ()
>   let thisBuffer =  case (t,e,e2) of 
>                         (Nothing, Nothing, Nothing)->NoBOp
>                         (Nothing, _, Just True)->SetBufferPlayStatus False NoBOp
>                         (Nothing, _, Just False)->SetBufferPlayStatus True NoBOp
>                         (Nothing, Just _, Nothing)->buf
>                         (Just x, _, _)->SetBufferTempo x NoBOp
>   (maybeMsgs, bb)<-eventBuffer -< thisBuffer
>   display-<bb
>   midiOut-<(dev, maybeMsgs)

> m1 = let m = c 4 qn :=: e 4 hn :=: g 4 dhn
>      in m:+:revM m
> m2 = let m = c 4 qn :+: e 4 qn :+: g 4 qn
>      in m:+:revM m
> testBuf1 = musicToBO False [] $ toMusic1 m1
> testBuf2 = musicToBO False [] $ toMusic1 m2

> testMUI1 = runMUI' $ getPlayMUI testBuf1
> testMUI2 = runMUI' $ getPlayMUI testBuf2
