-- | BeatCount consists of the three ways in counting a cycle. Dhruta is typically 2 beats, while
-- Anudhruta is a single beat. The Jati of the Thala defines the Dhruta
data BeatCount = Laghu | Dhruta | Anudhruta

-- | Common class to define both the Jati and Gati
data JatiGati = Tisra | Chaturasra | Khanda | Misra | Sankirna deriving (Show, Eq)

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
