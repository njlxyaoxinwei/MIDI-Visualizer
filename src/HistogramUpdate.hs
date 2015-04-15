-- Module        :  HistogramUpdate
-- Copyright     :  (c) Daniel Winograd-Cort 2015
-- Date Modified :  4/14/2015
-- 
-- A modified version of the histogram widget that does not automatically 
-- normalize its inputs.  This will likely be pushed into UISF soon.

module HistogramUpdate where

import FRP.UISF
import FRP.UISF.UITypes (Layout)
import FRP.UISF.SOE (nullGraphic, polyline, translateGraphic, withColor, text, Graphic)
import FRP.UISF.Widget (mkWidget, (//))

-- | The histogram widget creates a histogram of the input map.  It assumes 
-- that the elements are to be displayed linearly and evenly spaced.
histogram' :: (RealFrac a) => Layout -> UISF (SEvent [a]) ()
histogram' layout = 
  mkWidget Nothing layout process draw
  where process Nothing Nothing  _ _ = ((), Nothing, False)
        process Nothing (Just a) _ _ = ((), Just a, False) --TODO check if this should be True
        process (Just a) _       _ _ = ((), Just a, True)
        draw (xy, (w, h)) _ (Just lst@(_:_)) = translateGraphic xy . polyline . mkPts $ lst
          where mkPts l  = zip (reverse $ xs $ length l) (map adjust . {-normalize . -} reverse $ l)
                xs n     = let k = n-1 in 0 : map (\x -> truncate $ fromIntegral (w*x) / fromIntegral k) [1..k]
                adjust i = buffer + truncate (fromIntegral (h - 2*buffer) * (1 - i))
--                normalize lst = map (/m) lst where m = maximum lst
                buffer = truncate $ fromIntegral h / 10
        draw _ _ _ = nullGraphic


-- | The histogramWithScale widget creates a histogram and an x coordinate scale.
histogramWithScale' :: RealFrac a => Layout -> UISF (SEvent [(a,String)]) ()
histogramWithScale' layout = 
  mkWidget Nothing layout process draw
  where process Nothing Nothing  _ _ = ((), Nothing, False)
        process Nothing (Just a) _ _ = ((), Just a, False) --TODO check if this should be True
        process (Just a) _       _ _ = ((), Just a, True)
        draw (xy, (w, h)) _ = mymap (polyline . mkPts) mkScale
          where mkPts l  = zip (reverse $ xs $ length l) (map adjust . {-normalize . -} reverse $ l)
                xs n     = let k  = n-1
                               w' = w - sidebuffer * 2
                           in sidebuffer : map (\x -> sidebuffer + (truncate $ fromIntegral (w'*x) / fromIntegral k)) [1..k]
                adjust i = bottombuffer + truncate (fromIntegral (h - topbuffer - bottombuffer) * (1 - i))
                --normalize lst = map (/m) lst where m = maximum lst
                topbuffer = truncate $ fromIntegral h / 10
                bottombuffer = max 20 topbuffer
                sidebuffer = 80
                mkScale l = foldl (\pg (x,s) -> translateGraphic xy (withColor Black (text (x-((8*length s) `div` 2), h-16) s)) // pg) 
                                  nullGraphic $ zip (xs $ length l) l
                mymap :: ([a] -> Graphic) -> ([String] -> Graphic) -> SEvent [(a,String)] -> Graphic
                mymap f g (Just lst@(_:_)) = let (fl,gl) = unzip lst in g gl // translateGraphic xy (f fl)
                mymap _ _ _ = nullGraphic
