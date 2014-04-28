
{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts #-}

module Music.Graphics.Diagrams (
        -- draw,
        -- writeGraphic,
        -- openGraphic
) where

import Music.Score
import Music.Pitch
import Diagrams.Prelude hiding (Time, Duration)
import qualified Diagrams.Backend.SVG as SVG

-- test       
import Music.Prelude.Basic
import Control.Lens
import System.Process
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as ByteString
-- 
-- 
-- timeToDouble :: Time -> Double
-- timeToDouble = realToFrac . (.-. origin)
-- durationToDouble :: Duration -> Double
-- durationToDouble = realToFrac
-- pitchToDouble :: Music.Pitch.Pitch -> Double
-- pitchToDouble = realToFrac . semitones . (.-. c)
-- 
-- draw :: (Renderable (Path R2) b, Real a) => Score a -> Diagram b R2
-- draw = bg whitesmoke . scaleX 20{-TODO-} . mconcat . fmap drawNote . fmap (map1 timeToDouble . map2 durationToDouble . map3 realToFrac) . (^. events)
--     where
--         map1 f (a,b,c) = (f a,b,c)
--         map2 f (a,b,c) = (a,f b,c)
--         map3 f (a,b,c) = (a,b,f c)
--         drawNote (t,d,x) = translateY x $ translateX (t.+^(d^/2)) $ scaleX d $ noteShape
--         noteShape = lcA transparent $ fcA (blue  `withOpacity` 0.5) $ square 1
-- 
-- writeGraphic :: FilePath -> Score Double -> IO ()
-- writeGraphic path x = do
--     let dia = draw x
--     let svg = renderDia SVG.SVG (SVG.SVGOptions (Dims 1800 (1800/20)) Nothing) dia
--     let bs  = renderSvg svg
--     ByteString.writeFile path bs
--         
-- openGraphic :: Score Double -> IO ()
-- openGraphic x = do
--     writeGraphic "test.svg" x
--     -- FIXME find best reader
--     system "open -a Firefox test.svg"
--     return ()









-- drawScores
--     :: (Integral p, p ~ Pitch b, HasPitch b, Voice b ~ NotePart, HasVoice b)
--     => Score b -> Score c -> Diagram SVG R2
-- drawScores notes cmds = notes1D <> notes2D <> cmdsD <> middleLines <> crossLines
--     where
--         notes1 = mfilter (\x -> getPartGroup (getVoice x) == 1) notes
--         notes2 = mfilter (\x -> getPartGroup (getVoice x) == 2) notes
-- 
--         notes1D     = mconcat $ fmap (drawNote 1) $ perform notes1
--         notes2D     = mconcat $ fmap (drawNote 2) $ perform notes2
--         cmdsD       = mconcat $ fmap drawCmd $ perform cmds
--         middleLines = translateX ((/ 2) $ totalDur) (hrule $ totalDur)
--         crossLines  = mconcat $ fmap (\n -> translateX ((totalDur/5) * n) (vrule 100)) $ [0..5]
-- 
--         drawNote n (t,d,x) = translateY (getP x + off n) $ translateX (getT (t.+^(d^/2))) $ scaleX (getD d) $ noteShape n
--         off 1 = 50
--         off 2 = (-50)
--         drawCmd (t,d,x) = translateY 0 $ translateX (getT t) $ cmdShape
-- 
--         noteShape 1 = lcA transparent $ fcA (blue  `withOpacity` 0.3) $ square 1
--         noteShape 2 = lcA transparent $ fcA (green `withOpacity` 0.3) $ square 1
--         cmdShape = lcA (red `withOpacity` 0.3) $ vrule (200)
-- 
--         totalDur = getD $ duration notes
--         getT = fromRational . toRational
--         getD = fromRational . toRational
--         getP = (subtract 60) . fromIntegral . getPitch