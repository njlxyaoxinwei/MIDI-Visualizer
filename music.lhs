> module Visualize.Music where
> import Euterpea
> import Euterpea.IO.MUI.MidiWidgets
> import Codec.Midi

> midiToMsgs :: Midi->[(DeltaT, MidiMessage)]
> midiToMsgs (Midi _ tdv tracks) = [(0,ANote 0 48 100 2), (0.7, ANote 0 51 100 1.3), (1.4, ANote 0 55 100 0.5)]


> midiToBO :: Midi->BufferOperation MidiMessage
> midiToBO mid = let (m, _, _) = fromMidi mid
>                in musicToBO False [] m
