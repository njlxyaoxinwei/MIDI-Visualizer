> {-# LANGUAGE Arrows #-}

> module Visualize.Display where
> import Visualize.Music
> import Euterpea
> import Codec.Midi

Process an Event of Messages, a wrapper around Visualize.Music.groupMsgs

> groupMsgEvents :: SEvent [Message]->(SEvent [Message], [SEvent [Message]])
> groupMsgEvents Nothing     = let (_,cs) = groupMsgs []
>                              in (Nothing, map (const Nothing) cs)
> groupMsgEvents (Just msgs) = let (ss, cs) = groupMsgs msgs
>                              in (helper ss, map helper cs) where
>   helper [] = Nothing
>   helper xs = Just xs

> displayChannels :: [Channel]->UISF [SEvent [Message]] ()
> displayChannels []     = arr (const ())
> displayChannels (c:cs) = proc msgs -> do 
>   displayMessages    -< msgs!!c
>   displayChannels cs -< msgs


Display the messages event

> displayMessages :: UISF (SEvent [Message]) ()
> displayMessages = proc msgs -> do 
>   display <<< hold [] -< msgs


