> module Visualize.Music where
> import Euterpea
> import Euterpea.IO.MUI.MidiWidgets
> import Codec.Midi

> midiToMsgs :: Midi->[(DeltaT, MidiMessage)]
> midiToMsgs mid = let bop = midiToBO mid
>                  in case bop of
>                       AppendToBuffer x -> x
>                       _                -> error "File not supported"


> midiToBO :: Midi->BufferOperation MidiMessage
> midiToBO mid = let (m, _, _) = fromMidi mid
>                in musicToBO False [] m
