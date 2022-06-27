import System.Random
import System.CPUTime

-- | BeatCount consists of the three ways in counting a cycle. Dhruta is typically 2 beats, while
-- Anudhruta is a single beat. The Jati of the Thala defines the Dhruta
data BeatCount = Laghu | Dhruta | Anudhruta deriving(Read)

-- | Define conventional notation used to represent the Thalas
instance Show BeatCount where
    show Laghu = " | "
    show Dhruta = " O "
    show Anudhruta = " U "

-- | Thala is a combination of BeatCounts
newtype Thala = Thala [BeatCount] deriving (Read)

-- | Standard 7 thalas in the Suladi Sapta Thala system
dhruva = Thala [Laghu, Dhruta, Laghu, Laghu]
matya = Thala [Laghu, Dhruta, Laghu]
rupaka = Thala [Dhruta, Laghu]
jhampe = Thala [Laghu, Dhruta, Anudhruta]
thriputa = Thala [Laghu, Dhruta, Dhruta]
atta = Thala [Laghu, Laghu, Dhruta, Dhruta]
eka = Thala [Laghu]

-- | Define display of Thala based on that of the BeatCount
instance Show Thala where
    show (Thala []) = show ""
    show (Thala (x:xs)) = show x ++ show (Thala xs)

-- | Common class to define both the Jati and Gati
data JatiGati = Tisra | Chaturasra | Khanda | Misra | Sankirna deriving (Show, Eq, Read)

-- | Define instance of enum class by enumerating each Jati / Gati to be the number of beats they represent
instance Enum JatiGati where
    fromEnum a
        | a == Tisra = 3
        | a == Chaturasra = 4
        | a == Khanda = 5
        | a == Misra = 7
        | otherwise =  9
    toEnum a
        | a == 3 = Tisra
        | a == 4 = Chaturasra
        | a == 5 = Khanda
        | a == 7 = Misra
        | a == 9 = Sankirna
        | otherwise = error "Unspecified Length"

-- | Define a class for all syllables used in Konnakol
data Syllable = Tha | Ki | Ta |Di | Dhi | Gi |Jho | Na | Thom |Lan |Gu | Dhin | Ku | Ri | Ka | Tham | Langu | Thak |Dhim | Nam |Mi |Nu| Gdot | Gsc deriving ( Eq)

-- | Show instance for syllables, to display the gaps using dots
instance Show Syllable where
    show Gdot = "."
    show Gsc = ";"
    show Tha = "Tha"
    show Ki = "Ki"
    show Ta = "Ta"
    show Dhi = "Dhi"
    show Gi = "Gi"
    show Jho = "Jho"
    show Na = "Na"
    show Thom = "Thom"
    show Lan = "Lan"
    show Gu = "Gu"
    show Dhin = "Dhin"
    show Ku = "Ku"
    show Ri = "Ri"
    show Ka = "Ka"
    show Tham = "Tham"
    show Thak ="Thak"
    show Dhim = "Thak"
    show Nam = "Nam"
    show Mi = "Mi"
    show Nu = "Nu"
    show Di = "Di"
    show _ = ""

-- | Convert syllables into equivalent lengths
toNum::Syllable -> Int
toNum Langu= 2
toNum Gsc = 2
toNum _ = 1

-- | Define phrase as a collection of syllables, to faciliate show instance
newtype Phrase = Phrase [Syllable]

-- | Convert a phrase into its total length
toNumP::[Syllable] -> Int
toNumP [] = 0
toNumP ((x:xs)) = toNum x + toNumP xs

-- | Show instance for a phrase
instance Show Phrase where
    show (Phrase []) = ""
    show (Phrase (x:xs)) = show x ++ show (Phrase xs)

-- | Define Composition as collection of phrases with changes in speeds
data Comp = Composition [([Syllable], Int)] | KalaCh JatiGati deriving(Show)

-- | Method to show the Thala in the form Notation - Count - Gati
showFinalThala :: JatiGati -> Thala -> JatiGati -> String
showFinalThala jati thala gati =
    let a = show thala
        b = " (" ++ show ( calculateCount jati thala) ++ ")"
        c = " <" ++ show (fromEnum gati )++ ">"
    in a ++ b ++ c

-- | Get the Counts per beat in a certain thala in a particular gati
getCountPerBeat::JatiGati->Int->Int
getCountPerBeat gati maxS
  | gati == Chaturasra = 2^(maxS - 1)
  | maxS == 1 = 1
  | otherwise = fromEnum gati * 2^ max (maxS-2) 0

-- | Method to calculate number of beats in a Thala based on its jati
calculateCount :: JatiGati -> Thala -> Int
calculateCount _ (Thala []) = 0
calculateCount g (Thala (x:xs)) =
    case x of Laghu -> fromEnum g + calculateCount g (Thala xs)
              Dhruta -> 2 + calculateCount g (Thala xs)
              Anudhruta -> 1 + calculateCount g (Thala xs)

-- | Method to get appropriate symbols for representation
getThalaSplitPoints :: JatiGati -> Thala  -> [String]
getThalaSplitPoints _ (Thala [])  = []
getThalaSplitPoints j (Thala (x:xs))  =
    case x of Laghu -> [" | "] ++ replicate (fromEnum j - 1) "^" ++ getThalaSplitPoints j (Thala xs)
              Dhruta -> " O ":" ^ ": getThalaSplitPoints j (Thala xs)
              Anudhruta ->" U " : getThalaSplitPoints j (Thala xs)

-- | Representing Compositions with changing speeds
getRepresentation:: [Comp] -> JatiGati ->Thala ->Int->String
getRepresentation ((KalaCh x):y:xs) jati thala pos  =
     let (a,b) = getStringComp y jati thala (KalaCh x) pos
     in "<" ++ show (fromEnum x) ++ ">" ++ a ++ getRepresentation xs jati thala b
getRepresentation _ _ _ _ = ""

-- | Driver function for getting a virtual representation of a composition, after validation
getStringComp :: Comp->JatiGati->Thala->Comp->Int-> (String, Int)
getStringComp (Composition k) jati (Thala thala) (KalaCh gati) pos =
    let maxS = maximum $ map snd k
        countPerBeat = getCountPerBeat gati maxS
        b = calculateCount jati (Thala thala)
        countPerAvarta = countPerBeat * b
        a = convToList k countPerBeat gati
        d =
            -- if mod (length a) countPerBeat == 0 then
            let c = replicate (calculateCount jati (Thala thala)) countPerBeat
                e = getThalaSplitPoints jati (Thala thala)
                in (finalDisp a (Thala thala) c pos countPerBeat e, pos + div (mod (length a) countPerAvarta) countPerBeat )
            --else ("Error", 0)
    in d
getStringComp _ _ _ _ _ = ("",0)

-- | Core function in obtaining string from Composition
convToList :: [([Syllable], Int)] -> Int ->JatiGati-> [Syllable]
convToList [] maxs g= []
convToList listas maxs g =
   concatMap (\(x,s)-> concatMap (\t -> if t == Langu then Lan:Gu:replicate (div maxs (getCountPerBeat g s)   - 2) Gdot
   else if  t== Gsc then replicate (div maxs (getCountPerBeat g s))  Gdot
   else t:replicate (div maxs (getCountPerBeat g s) -1) Gdot) x) listas

-- | Final display of a thala in lines with proper subdivisions
finalDisp :: [Syllable] ->Thala -> [Int] -> Int ->Int->[String]-> String
finalDisp s (Thala thala) arr n cPB e =
    if null s then ""
    else let pos = mod n (length arr)
             c = e!!pos
             b = if pos == length arr - 1 then "||\n" else ""
        in c ++ show (Phrase (take cPB s)) ++ b ++ finalDisp (drop (arr !! pos) s) (Thala thala) arr (n+1) cPB e

-- | Define the standard phrases for different lengths
phrase4len :: Int -> [[Syllable]]
phrase4len 1 = [[Tha], [Dhi], [Thom], [Nam]]
phrase4len 2 = [[Tha, Ka], [Ki, Ta], [Dhi, Mi], [Tha, Ri], [Di, Na], [Gi, Na], [Jho, Nu]]
phrase4len 3 = [[Tha, Ki, Ta], [Tha, Di, Mi], [Tha, Tha, Ka], [Dhi, Na, Ka]]
phrase4len 4 = [[Ki,Ta, Tha, Ka], [Tha, Ri, Ki, Ta], [Tha, Ka, Di, Na], [Tha, Ka, Dhi, Mi], [Tha, Ka, Jho, Nu], [Tha, Lan, Gdot, Gu],
                [Gi, Na, Ki, Ta], [Tha, Di, Mi, Tha]]
phrase4len 5 = [[Tha, Di, Gi, Na, Thom], [Tha, Ka, Tha, Ki, Ta], [Tha, Ka, Tha, Di, Mi], [Tha, Dhi, Mi, Tha, Ka],
                 [Dhi, Na, Ka, Dhi, Mi],[Tha, Tha, Ka, Dhi, Mi]]
phrase4len 6 = [Tha, Dhi, Gdot, Gi, Na, Thom] : [ x++ y | x <- phrase4len 2, y <- phrase4len 4] ++
                [ x++ y | x <- phrase4len 4, y <- phrase4len 2] ++[ x++ y | x <- phrase4len 3, y <- phrase4len 3]
phrase4len 7 = [Tha, Gdot, Dhi, Gdot, Gi, Na, Thom]: [ x++ y | x <- phrase4len 3, y <- phrase4len 4] ++ [ x++ y | x <- phrase4len 4, y <- phrase4len 3]
                ++ [ x++ y | x <- phrase4len 2, y <- phrase4len 5] ++ [ x++ y | x <- phrase4len 5, y <- phrase4len 2]
phrase4len 8 = [[Tha, Dhi, Gdot, Gi, Gdot, Na, Gdot, Thom], [Tha, Ka, Tham, Gdot, Tha, Ri, Ki, Ta],[Dhi,Gsc, Gdot, Tham, Gdot, Tha, Ka], [Dhi, Ku, Tha, Ri, Ki, Ta, Tha, Ka]] ++ [ x ++ y ++ z | x <- phrase4len 2, y <- phrase4len 3, z <- phrase4len 3]
                 ++ [ x++ y | x <- phrase4len 4, y <- phrase4len 4] ++ [ x++ y ++z| x <- phrase4len 3, y <- phrase4len 2, z<- phrase4len 3]
                 ++ [ x++ y ++ z  | x <- phrase4len 3, y <- phrase4len 3, z<-phrase4len 2]
phrase4len 9 = [Tha, Gdot, Dhi, Gdot, Gi, Gdot, Na, Gdot, Thom]: [ x++ y | x <- phrase4len 4, y <- phrase4len 5] ++
             [ x++ y | x <- phrase4len 5, y <- phrase4len 4] ++ [ x++ y ++ z  | x <- phrase4len 3, y <- phrase4len 3, z<-phrase4len 3]
             ++ [ x++ y ++ z  | x <- phrase4len 2, y <- phrase4len 3, z<-phrase4len 4]
phrase4len _ = [[]]

-- | Function to randomly select a phrase of a specified length. Defined recursively for phrases
-- of length greater than 9

phraseGenerator::Int-> StdGen-> ([Syllable], StdGen)
phraseGenerator x gen =
    if x < 10 then let (y,z) =randomR (0, length(phrase4len x) - 1) gen
                    in  (phrase4len x !! y, z)
        else let (a, newgen) = randomR (2, x - 2) gen
                 (b, c) = genPhrase4Me a newgen
                 (d, e) = genPhrase4Me (x - a) c
            in if a > x - a then (d++b, e) else (b++d, e)

-- | Function to decide whether the phrase has to be generated with breaks or without them
genPhrase4Me ::Int-> StdGen-> ([Syllable], StdGen)
genPhrase4Me x tossgen =
  let (a,gen) = randomR (1,10) tossgen ::(Int, StdGen)
  in if a <= 7||x == 1
    then phraseGenerator x gen
    else let factor = head (filter (\y->mod x y==0) [2,3..])

             (pha, c) = phraseGenerator (div x factor) gen
             b = concatMap (\x->x:replicate (factor - 1) Gdot) pha
        in (b,c)

-- | To randomly generate different phrases
genValues [] t = []
genValues (x:xs) t =
    let newgen =  t !! x
        genPhrase = genPhrase4Me x (mkStdGen newgen)
    in fst genPhrase:genValues xs (drop x t)

-- | Define the constant phrase of a Mohra with respect to the given length
mohrad::JatiGati -> [Syllable]
mohrad Tisra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot,Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot]
mohrad Chaturasra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Tha, Lan, Gdot,Gu, Dhin, Gdot, Gdot, Gdot]
mohrad Khanda = [Tha, Lan, Gdot, Gu, Dhin, Tha, Lan, Gdot, Gu, Dhin]
mohrad Misra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak,Gdot, Tha, Lan, Gdot,Gu, Dhin, Gdot ]
mohrad Sankirna = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Tha, Lan, Gdot, Gu, Dhin, Gdot, Dhin, Gdot, Gdot, Gdot]

-- | Define one of the final constants used in the Mohra
mohraC1 Chaturasra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Gdot, Gdot]
mohraC1 Tisra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot]
mohraC1 Khanda = [Tha, Lan, Gdot, Gu, Dhin]
mohraC1 Misra = [Tha, Lan, Gdot, Gu, Dhin,Gdot, Gdot]
mohraC1 Sankirna = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Gdot, Gdot, Gdot]

-- | Define the constant used to end the Mohra
mohraC2 Tisra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot, Gu, Gdot]
mohraC2 Chaturasra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot]
mohraC2 Khanda = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Gdot, Gdot ]
mohraC2 Misra = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot]
mohraC2 Sankirna = [Tha, Lan, Gdot, Gu, Dhin, Gdot, Thak, Gdot, Dhin, Gdot, Gdot, Gdot, Gu, Gdot]

-- | To obtain the right speed for generation of Mohra with respect to its gati/ nadai
getMohraSpeed::JatiGati->Int
getMohraSpeed gati
    | gati == Chaturasra = 4
    | gati == Tisra = 4
    | otherwise = 2

-- | To get the appropriate separations for a required mohra generation
getMohraSeparation::Int->JatiGati->[Int]
getMohraSeparation count gati =
    let snd = length $ mohrad gati
        fst =div (count - 2*snd) 2
    in [fst, snd, fst, snd]

-- | To generate a Mohra on the basis of the jati, thala and gati
genMohra::JatiGati->Thala->JatiGati->StdGen->String
genMohra jati thala gati gen=
    let sp = getMohraSpeed gati
        phd = mohrad gati
        overAllCount = if calculateCount jati thala<= 4 then 2*calculateCount jati thala
                        else calculateCount jati thala
        [a,b,c,d] = getMohraSeparation (getCountPerBeat gati sp*overAllCount) gati
        (pha, gen1) = genPhrase4Me a gen
        (phb, gen2 ) = genPhrase4Me b gen1
        c1 = mohraC1 gati
        c2 = mohraC2 gati
        derMohra = concat [pha, phb, pha, phd, pha, phb, pha, phd, pha, phb, pha, c1,pha, c1, pha,c2, c2, c2]
    in getRepresentation [KalaCh gati, Composition [(derMohra, sp)]] jati thala 0

-- To read the thala from user input
getThala :: String->Thala
getThala "Dhruva" = dhruva
getThala "Matya" = matya
getThala "Eka" = eka
getThala "Rupaka" = rupaka
getThala "Thriputa" = thriputa
getThala "Atta" = atta
getThala "Jhampe" = jhampe

-- To generate the purvardha for a given length for the Korvai
getPurvardha::Int->StdGen->([Syllable], StdGen)
getPurvardha sum gen =
    let vals1 = [(x,d, g) | x <- [1,2..sum] , d<- [0, 1..8], g<-[0,1..8], 3*x + 3*d + 3*g== sum]
        vals2 = [(x,d, g) | x <- [1,2..sum] , d<- [-1,-2..(-8)], g<-[0,1..8], 3*x + 3*d + 3*g == sum && x + d >0 && x + 2*d >0]
        vals = vals1 ++ vals2
        ((pos, gen1), ind) = if null vals then
            let val = [(x,d,g) | x<- [1,2..sum], d<-[1,2..4],g<-[0,1..8], x + x *d + x*d*d + 3*g == sum]
                in (randomR (0,length val - 1 ) gen :: (Int, StdGen), 1)
                else (randomR (0,length vals - 1 ) gen :: (Int, StdGen), 0)
        (x,d, g) = if ind == 0 then vals!!pos else [(x,d,g) | x<- [1,2..sum], d<-[1,2..4],g<-[0,1..8], x + x *d + x*d*d + 3*g == sum]!!pos
        l2 = if ind == 0 then x + d  else x*d
        l3 = if ind == 0 then x + 2 *d else x * d * d
        (phr1, gen2) = genPhrase4Me x gen1
        (phr2, gen3) = genPhrase4Me l2 gen2
        (phr3, gen4) = genPhrase4Me l3 gen3
        phr4 = if g==0 then [] else Dhi:replicate (g - 1) Gdot
    in (phr1 ++ phr4 ++ phr2  ++ phr4 ++ phr3  ++ phr4, gen)

-- To generate an Uttarardha which has a fixed structure
getUttar :: Int-> [Syllable]
getUttar len =
    let factor = head (filter (\y->mod (len - 2 * y) 3==0 && (len - 2*y) <= 30) [0,1..])
        gapPhrase = if factor == 0  then [] else Dhin:replicate (factor - 1) Gdot
        lenphrase = div (len - 2*factor) 3
        mainPhrase = head (phrase4len lenphrase)
    in mainPhrase ++ gapPhrase ++ mainPhrase ++ gapPhrase ++ mainPhrase

-- To generate an Uttarardha which has a varying structure
getUttarVarying :: Int -> [Syllable]
getUttarVarying len =
    let factor = head (filter (\y->mod (len - 2 * y) 6==0 && (len - 2*y) <= 30 ) [1..])
        gapPhrase = if factor == 0  then [] else Dhin:replicate (factor - 1) Gdot
        lenphrase = div (len - 2*factor) 6
        mainPhrase = head (phrase4len lenphrase)
    in mainPhrase ++ gapPhrase ++ mainPhrase ++ mainPhrase ++ gapPhrase ++ mainPhrase ++ mainPhrase ++ mainPhrase

-- Core function to generate a desired Korvai
genKorvai::JatiGati -> Thala -> JatiGati -> StdGen -> String
genKorvai jati thala gati gen=
    let sp = getMohraSpeed gati
        overallCount = let a = if gati == Chaturasra then 1 else 2 in a * calculateCount jati thala*getCountPerBeat gati sp
        (totPur, gen1) = randomR (overallCount - 40, overallCount - 15) gen :: (Int, StdGen)
        totUtt = overallCount -  totPur
        (purva, gen2) =let a = if gati == Chaturasra then 2 else 1 in getPurvardha (a *totPur) gen1
        uttara = let a = if gati == Chaturasra then 1 else 0 in concatMap (\x->x:replicate a Gdot) (if even totUtt && totUtt >=32 then getUttarVarying totUtt else getUttar totUtt)
        in getRepresentation [KalaCh gati, Composition [(purva++uttara, sp)]] jati thala 0

main = do
    value1 <- getLine
    value2 <- getLine
    value3 <- getLine
    value4 <- getLine
    gen <- getStdGen
    let
        x = (read value1::JatiGati)
        y = getThala value2
        z = (read value3:: JatiGati)
        ch = (read value4 :: Int)
        w = if ch == 1 then genKorvai x y z gen else genMohra x y z gen
    putStrLn w

