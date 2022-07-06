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
main = main2

main3 = mainWith $pictureMohra Chaturasra thriputa Chaturasra
main2 = mainWith $ gridKon $ toColors [P 5,G 6, P 5, G 6, P 5, G 6, P 5,G 4, P 5, G 4, P 5, G 4, P 5,G 2, P 5, G 2, P 5, G 2,
                                    P 5, P 5, P 5, G 1, P 5, P 5, P 5, G 1, P 5, P 5, P 5 ] 32

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

pictureMohra :: JatiGati -> Thala -> JatiGati -> Diagram B
pictureMohra jati thala gati =
    let sp = getMohraSpeed gati
        phd = mohrad gati
        overAllCount = if calculateCount jati thala<= 4 then 2*calculateCount jati thala
                        else calculateCount jati thala
        [a,b,c,d] = getMohraSeparation (getCountPerBeat gati sp*overAllCount) gati
        lenC1 = length $ mohraC1 gati
        lenC2 = length $ mohraC2 gati
        mohraRep = concatMap sepToSinglesM ([(a, 1), (b, 2), (c,3), (d, 4), (a, 1), (b, 2), (c, 3), (d, 4), (a, 1), (b, 2), (c, 3), (lenC1, 5),
                                      (a, 1), (lenC1, 5), (a, 1), (lenC2, 5)]::[(Int,Int)])
        avarta = getCountPerBeat gati sp * calculateCount jati thala
        colors = chunksOf avarta mohraRep
    in gridKonM colors

sepToSinglesM::(Int, Int) -> [(Double, Int)]
sepToSinglesM (a,y) = map (\x -> (fromIntegral x/fromIntegral a,y) ) [1..a]

getSquaresM::(Double, Int) -> Diagram B
getSquaresM (x, 1) = rect 1 1 # lw thin # fc (rgb x 1 x)
getSquaresM (x, 2) = rect 1 1 # lw thin # fc (rgb x 0 x)
getSquaresM (x, 3) = rect 1 1 # lw thin # fc (rgb x x 1)
getSquaresM (x, 4) = rect 1 1 # lw thin # fc (rgb x x 0)
getSquaresM (x, 5) = rect 1 1 # lw thin # fc (rgb 1 x x)

gridKonM :: [[(Double, Int)]] -> Diagram B
gridKonM x = lattice
  where
      y = length x
      grids = map (centerXY.hcat. map getSquaresM) x
      lattice = vcat grids