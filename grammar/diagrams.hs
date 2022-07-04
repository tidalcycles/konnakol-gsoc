{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Diagrams.Prelude
    ( lw, thin, centerXY, fc, hcat, vcat, rect, (#), Diagram )
import Diagrams.Backend.SVG.CmdLine
import Data.List.Split ( chunksOf )
import Data.Colour.SRGB.Linear
import qualified Data.Data as Data.Typeable.Internal
import Define hiding (getNums, main)

getSquares::(Double, Double) -> Diagram B
getSquares (x, y) = if x == 0 then rect 1 1 # lw thin # fc (rgb 1 (y/20) (y/20))
                  else rect 1 1 # lw thin # fc (rgb 0 (y/ 10) (y/ 10))



gridKon :: [[(Double, Double)]] -> Diagram B
gridKon x = lattice
  where
      y = length x
      grids = map (centerXY.hcat. map getSquares) x
      lattice = vcat grids

main1 = mainWith $ gridKon $ toColors [P 7, G 4, P 7, G 4, P 7, G 4, P 6, G 4, P 6, G 4, P 6, G 4,
                                                P 5, G 4, P 5, G 4, P 5, G 4,P 4, G 4, P 4, G 4, P 4, G 4,
                                                 P 3, G 4, P 3, G 4, P 3, G 4,
                                                P 2, G 4, P 2, G 4, P 2, G 4, P 1, G 4, P 1, G 4, P 1, G 3, P 1,
                                                G 4, P 1, G 4, P 1, G 3, P 1, G 4, P 1 , G 4, P 1] 32

main = main1


-- Single gati compositions
-- After obtaining the composition, calculate number of avartaas
-- Number of rows = Number of avartas
-- Divide each row into countperavarta
-- For each section, first calculate number of counts it is taking


-- | To convert a simplified Korvai to its numerical equivalent
getNums::[JustNums]->[Double]
getNums [] = []
getNums ((P a):xs) = fromIntegral a:getNums xs
getNums ((G a) : xs) =fromIntegral  a:getNums xs
getNums (B:xs) = 0:getNums xs

sepToSingles::[JustNums] -> [(Double, Double)]
sepToSingles [] =[]
sepToSingles (B : xs) = sepToSingles xs
sepToSingles ((P a):xs) = replicate a (1, fromIntegral a) ++ sepToSingles xs
sepToSingles ((G a):xs) = replicate a (0, fromIntegral a) ++ sepToSingles xs

toColors::[JustNums] -> Int -> [[(Double, Double)]]
toColors xs n = chunksOf n $ sepToSingles xs

