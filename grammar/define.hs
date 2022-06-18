import System.Random

-- | BeatCount consists of the three ways in counting a cycle. Dhruta is typically 2 beats, while
-- Anudhruta is a single beat. The Jati of the Thala defines the Dhruta
data BeatCount = Laghu | Dhruta | Anudhruta

-- | Common class to define both the Jati and Gati
data JatiGati = Tisra | Chaturasra | Khanda | Misra | Sankirna deriving (Show, Eq)
-- | Define a class for all syllables used in Konnakol
data Syllable = Tha | Ki | Ta |Di | Dhi | Gi |Jho | Na | Thom |Lan |Gu | Dhin | Ku | Ri | Ka | Tham | Langu | Thak |Dhim | Nam |Mi |Nu| Gdot | Gsc deriving ( Eq)

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
phrase4len 7 = [Tha, Gdot, Tha, Gdot, Gi, Na, Thom]: [ x++ y | x <- phrase4len 3, y <- phrase4len 4] ++ [ x++ y | x <- phrase4len 4, y <- phrase4len 3]
                ++ [ x++ y | x <- phrase4len 2, y <- phrase4len 5] ++ [ x++ y | x <- phrase4len 5, y <- phrase4len 2]
phrase4len 8 = [Tha, Dhi, Gdot, Gi, Gdot, Na, Gdot, Thom] : [ x ++ y ++ z | x <- phrase4len 2, y <- phrase4len 3, z <- phrase4len 3]
                 ++ [ x++ y | x <- phrase4len 4, y <- phrase4len 4] ++ [ x++ y ++z| x <- phrase4len 3, y <- phrase4len 2, z<- phrase4len 3]
                 ++ [ x++ y ++ z  | x <- phrase4len 3, y <- phrase4len 3, z<-phrase4len 2]
phrase4len 9 = [Tha, Gdot, Dhi, Gdot, Gi, Gdot, Na, Gdot, Thom, Gdot]: [ x++ y | x <- phrase4len 4, y <- phrase4len 5] ++
             [ x++ y | x <- phrase4len 5, y <- phrase4len 4] ++ [ x++ y ++ z  | x <- phrase4len 3, y <- phrase4len 3, z<-phrase4len 3]
             ++ [ x++ y ++ z  | x <- phrase4len 2, y <- phrase4len 3, z<-phrase4len 4]

-- | Function to define phrase for phrases of length more than 9
genPhrase4Me ::Int-> StdGen-> ([Syllable], StdGen)
genPhrase4Me x gen =
  if x < 10 then let (y,z) =randomR (0, length(phrase4len x) - 1) gen
                    in  (phrase4len x !! y, z)
    else let (a, newgen) = randomR (2, x - 2) gen
             (b, c) = genPhrase4Me a newgen
             (d, e) = genPhrase4Me (x - a) c
            in if a > x - a then (d++b, e) else (b++d, e)

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

-- | Define phrase as a collection of syllables, to faciliate show instance
newtype Phrase = Phrase [Syllable]

-- | Show instance for a phrase
instance Show Phrase where
    show (Phrase []) = ""
    show (Phrase (x:xs)) = show x ++ show (Phrase xs)

-- | Define Composition as collection of phrases with changes in speeds
data Comp = Composition [([Syllable], Int)] | KalaCh JatiGati


-- | Representing Compositions with changing speeds
getRepresentation:: [Comp] -> JatiGati ->Thala ->Int->  String
getRepresentation ((KalaCh x):y:xs) jati thala pos  =
     let (a,b) = getStringComp y jati thala (KalaCh x) pos
     in "<" ++ show (fromEnum x) ++ ">" ++ a ++ getRepresentation xs jati thala b
getRepresentation [] _ _ _ = ""

-- | Driver function for getting a virtual representation of a composition, after validation
getStringComp :: Comp->JatiGati->Thala->Comp->Int-> (String, Int)
getStringComp (Composition k) jati (Thala thala) (KalaCh gati) pos =
    let maxS = maximum $ map snd k
        countPerBeat = getCountPerBeat gati maxS
        b = calculateCount jati (Thala thala)
        countPerAvarta = countPerBeat * b
        a = convToList k countPerBeat gati
        d = if mod (length a) countPerBeat == 0 then
            let c = replicate (calculateCount jati (Thala thala)) countPerBeat
                e = getThalaSplitPoints jati (Thala thala) countPerBeat
                in (finalDisp a (Thala thala) c pos countPerBeat e, pos + div (mod (length a) countPerAvarta) countPerBeat )
            else ("Error", 0)
    in d
getStringComp _ _ _ _ _ = ("",0)



-- | Get the Counts per beat in a certain thala in a particular gati
getCountPerBeat::JatiGati->Int->Int
getCountPerBeat gati maxS
  | gati == Chaturasra = 2^(maxS - 1)
  | maxS == 1 = 1
  | otherwise = fromEnum gati * 2^ max (maxS-2) 0

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
             b = if pos == length arr - 1 then "\n" else ""
        in show (Phrase (take cPB s)) ++ c++ b ++ finalDisp (drop (arr !! pos) s) (Thala thala) arr (n+1) cPB e


-- | Method to calculate number of beats in a Thala based on its jati
getThalaSplitPoints :: JatiGati -> Thala  ->Int-> [String]
getThalaSplitPoints _ (Thala []) _ = []
getThalaSplitPoints j (Thala (x:xs)) cPB =
    case x of Laghu -> replicate (fromEnum j - 1) "^" ++ [" | "]  ++ getThalaSplitPoints j (Thala xs) cPB
              Dhruta -> "^":" O ": getThalaSplitPoints j (Thala xs) cPB
              Anudhruta ->" U " : getThalaSplitPoints j (Thala xs) cPB

-- | Convert syllables into equivalent lengths
toNum::Syllable -> Int
toNum Langu= 2
toNum Gsc = 2
toNum _ = 1

-- | Convert a phrase into its total length
toNumP::[Syllable] -> Int
toNumP [] = 0
toNumP ((x:xs)) = toNum x + toNumP xs

-- | Thala is a combination of BeatCounts
newtype Thala = Thala [BeatCount]

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

-- | Define conventional notation used to represent the Thalas
instance Show BeatCount where
    show Laghu = " | "
    show Dhruta = " O "
    show Anudhruta = " U "

-- | Define display of Thala based on that of the BeatCount
instance Show Thala where
    show (Thala []) = show ""
    show (Thala (x:xs)) = show x ++ show (Thala xs)

-- | Standard 7 thalas in the Suladi Sapta Thala system
dhruva = Thala [Laghu, Dhruta, Laghu, Laghu]
matya = Thala [Laghu, Dhruta, Laghu]
rupaka = Thala [Dhruta, Laghu]
jhampe = Thala [Laghu, Dhruta, Anudhruta]
thriputa = Thala [Laghu, Dhruta, Dhruta]
atta = Thala [Laghu, Laghu, Dhruta, Dhruta]
eka = Thala [Laghu]

-- | Method to calculate number of beats in a Thala based on its jati
calculateCount :: JatiGati -> Thala -> Int
calculateCount _ (Thala []) = 0
calculateCount g (Thala (x:xs)) =
    case x of Laghu -> fromEnum g + calculateCount g (Thala xs)
              Dhruta -> 2 + calculateCount g (Thala xs)
              Anudhruta -> 1 + calculateCount g (Thala xs)

-- | Method to show the Thala in the form Notation - Count - Gati
showFinalThala :: JatiGati -> Thala -> JatiGati -> String
showFinalThala jati thala gati =
    let a = show thala
        b = " (" ++ show ( calculateCount jati thala) ++ ")"
        c = " <" ++ show (fromEnum gati )++ ">"
    in a ++ b ++ c

-- | To randomly generate different phrases
genValues [] t = []
genValues (x:xs) t =
    let newgen =  t !! x
        genPhrase = genPhrase4Me x (mkStdGen newgen)
    in fst genPhrase:genValues xs (drop x t)

main = do
    gen <- getStdGen
    value <- getLine
    let t = randoms gen :: [Int]
        x = (read value::[Int])
        w = show $ genValues x t
    putStrLn w
   