> {-# LANGUAGE Arrows #-}

> module Play where
> import Euterpea
> import Codec.Midi
> import FRP.UISF.AuxFunctions


> playMid :: Midi->IO ()
> playMid = putStrLn.show



