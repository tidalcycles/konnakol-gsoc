{-# LANGUAGE DataKinds #-}
import Language.Haskell.TH.Ppr (where_clause)
-- | BeatCount consists of the three ways in counting a cycle. Dhruta is typically 2 beats, while
-- Anudhruta is a single beat. The Jati of the Thala defines the Dhruta
data BeatCount = Laghu | Dhruta | Anudhruta

-- | Common class to define both the Jati and Gati
data JatiGati = Tisra | Chaturasra | Khanda | Misra | Sankirna deriving (Show, Eq)

-- | Define a class for all syllables used in Konnakol
data Syllable = Tha | Ki | Ta | Dhi | Gi | Na | Thom | Dhin | Ku | Ri | Ka | Tham | Langu | Thak |Dhim | Nam |Mi | Gdot | Gsc deriving (Show, Eq)

-- | Define phrase as a collection of syllables
newtype Phrase = Phrase [Syllable]



-- | Define a composition following one certain Gati using a 4 - member tuple
newtype Composition = Composition ([(Phrase, Int)], JatiGati, Thala, JatiGati)

getStringComp::Composition->String
getStringComp (Composition ([(phr,speed)], jati, thala, gati)) =
    let maxSpeed = maximum $ map snd [(phr, speed)]
        b = calculateCount jati thala
        countPerAvarta = (2^(maxSpeed - 1)) * b * fromEnum gati
        a = convToList [(phr, speed)] maxSpeed
    in show(a)
getStrinComp _ = show ""


convToList :: [(Phrase, Int)] -> Int -> [String]
convToList [(Phrase x,s)] maxs =
   map (\(x,s)-> concatMap (\t -> if t == Langu || t== Gsc then show t ++ concat (replicate (2^(maxs - s - 1) - 1)  "-") 
   else show t ++ concat(replicate(2^(maxs - s) - 1) "-")) x) [(x,s)]

-- instance Basic Check for composition

-- further check for copmosition

-- | Convert syllables into equivalent lengths
toNum::Syllable -> Int
toNum Langu= 2
toNum Gsc = 2
toNum _ = 1

-- | Convert a phrase into its total length
toNumP::Phrase -> Int
toNumP (Phrase []) = 0
toNumP (Phrase (x:xs)) = toNum x + toNumP (Phrase xs)

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
    show Laghu = "| "
    show Dhruta = "O "
    show Anudhruta = "U "

-- | Define display of Thala based on that of the BeatCount
instance Show Thala where
    show (Thala [x]) = show x
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
