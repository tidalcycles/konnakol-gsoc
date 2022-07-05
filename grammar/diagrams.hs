{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Diagrams.Prelude
    ( lw, thin, centerXY, fc, hcat, vcat, rect, (#), Diagram, none )
import Diagrams.Backend.SVG.CmdLine
import Data.List.Split ( chunksOf )
import Data.Colour.SRGB.Linear
import qualified Data.Data as Data.Typeable.Internal
import Define hiding (getNums, main)
import System.Random

getSquares::Double -> Diagram B
getSquares x = if x >0  then rect 1 1 # lw thin # fc (rgb 1 x x)
                  else rect 1 1 # lw thin # fc (rgb 0 (1+x) (1+x))

gridKon :: [[Double]] -> Diagram B
gridKon x = lattice
  where
      y = length x
      grids = map (centerXY.hcat. map getSquares) x
      lattice = vcat grids

main1 = mainWith $ pictureKorvai Chaturasra thriputa Chaturasra (mkStdGen 758)
main = main1

main2 = mainWith $ gridKon $ toColors [P 7, G 4, P 7, G 4, P 7, G 4, P 6, G 4, P 6, G 4, P 6, G 4,
                                                P 5, G 4, P 5, G 4, P 5, G 4,P 4, G 4, P 4, G 4, P 4, G 4,
                                                 P 3, G 4, P 3, G 4, P 3, G 4,
                                                P 2, G 4, P 2, G 4, P 2, G 4, P 1, G 4, P 1, G 4, P 1, G 3, P 1,
                                                G 4, P 1, G 4, P 1, G 3, P 1, G 4, P 1 , G 4, P 1] 32

-- | To convert a simplified Korvai to its numerical equivalent
getNums::[JustNums]->[Double]
getNums [] = []
getNums ((P a):xs) = fromIntegral a:getNums xs
getNums ((G a) : xs) =fromIntegral  a:getNums xs
getNums (B:xs) = 0:getNums xs

sepToSingles::[JustNums] -> [Double]
sepToSingles [] =[]
sepToSingles (B : xs) = sepToSingles xs
sepToSingles ((P a):xs) = map (\x -> fromIntegral x/fromIntegral a) [1..a] ++ sepToSingles xs
sepToSingles ((G a):xs) = map (\x -> fromIntegral (-x + 1)/ fromIntegral a) [1..a] ++ sepToSingles xs

toColors::[JustNums] -> Int -> [[Double]]
toColors xs n = chunksOf n $ sepToSingles xs

pictureKorvai :: JatiGati -> Thala -> JatiGati -> StdGen ->Diagram B
pictureKorvai jati thala gati gen =
    let (_,korvai) = genKorvai jati thala gati gen
        sp = getMohraSpeed gati -1
        avarta = calculateCount jati thala*getCountPerBeat gati sp
        counts = if avarta < 50 then 4* avarta else avarta
        overallCount = 2* counts
        colors = toColors korvai avarta
    in gridKon colors