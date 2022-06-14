-- | BeatCount consists of the three ways in counting a cycle. Dhruta is typically 2 beats, while
-- Anudhruta is a single beat. The Jati of the Thala defines the Dhruta
data BeatCount = Laghu | Dhruta | Anudhruta

-- | Common class to define both the Jati and Gati
data JatiGati = Tisra | Chaturasra | Khanda | Misra | Sankirna deriving (Show, Eq)
-- | Define a class for all syllables used in Konnakol
data Syllable = Tha | Ki | Ta |Di | Dhi | Gi |Jho | Na | Thom |Lan|Gu| Dhin | Ku | Ri | Ka | Tham | Langu | Thak |Dhim | Nam |Mi | Gdot | Gsc deriving ( Eq)

instance Show Syllable where
    show Gdot = "."
    show Gsc = ";"
    show Tha = "Tha"
    show Ki = "Ki"
    show Ta = "ta"
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
    show _ = ""
 
newtype Phrase = Phrase [Syllable]

instance Show Phrase where
    show (Phrase []) = ""
    show (Phrase (x:xs)) = show x ++ show (Phrase xs)



-- | Define a composition following one certain Gati using a 4 - member tuple
newtype Composition = Composition ([([Syllable], Int)], JatiGati, Thala, JatiGati)

-- | Get the Counts per beat in a certain thala in a particular gati
getCountPerBeat::JatiGati->Int->Int
getCountPerBeat gati maxS
  | gati == Chaturasra = 2^(maxS - 1)
  | maxS == 1 = 1
  | otherwise = fromEnum gati * 2^ max (maxS-2) 0

-- | Driver function for getting a virtual representation of a composition, after validation
getStringComp::Composition->IO()
getStringComp (Composition (k , jati, thala, gati)) =
    let maxS = maximum $ map snd k
        countPerBeat = getCountPerBeat gati maxS
        b = calculateCount jati thala
        countPerAvarta = countPerBeat * b 
        a = convToList k countPerBeat gati
        d = if mod (length a) countPerAvarta == 0 then 
            let c = getThalaSplitPoints jati thala countPerBeat
                in finalDisp a thala c 0
            else "Error"
    in putStrLn
 d
getStrinComp _ = []

-- | Method to calculate number of beats in a Thala based on its jati
getThalaSplitPoints :: JatiGati -> Thala  ->Int-> [Int]
getThalaSplitPoints _ (Thala []) _ = []
getThalaSplitPoints j (Thala (x:xs)) cPB =
    case x of Laghu -> cPB*fromEnum j  : getThalaSplitPoints j (Thala xs) cPB
              Dhruta -> cPB * 2 : getThalaSplitPoints j (Thala xs) cPB
              Anudhruta ->cPB : getThalaSplitPoints j (Thala xs) cPB

-- | Core function in obtaining string from Composition
convToList :: [([Syllable], Int)] -> Int ->JatiGati-> [Syllable]
convToList [] maxs g= []
convToList listas maxs g =
   concatMap (\(x,s)-> concatMap (\t -> if t == Langu then Lan:Gu:replicate (div maxs (getCountPerBeat g s)   - 2) Gdot
   else if  t== Gsc then replicate (div maxs (getCountPerBeat g s))  Gdot
   else t:replicate (div maxs (getCountPerBeat g s) -1) Gdot) x) listas

-- | Final display of a thala in lines with proper subdivisions
finalDisp :: [Syllable] ->Thala -> [Int] -> Int -> String
finalDisp s (Thala thala) arr n =
    if null s then ""
    else let pos = mod n (length arr)
        in show (Phrase (take (arr !! pos) s)) ++ show (thala!!pos) ++ finalDisp (drop (arr !! pos) s) (Thala thala) arr (n+1)

-- instance Basic Check for composition

-- further check for copmosition

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
