data JatiGati = Tisra | Chaturasra | Khanda | Misra | Sankirna deriving (Show, Eq)

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

data BeatCount = Laghu | Dhruta | Anudhruta

instance Show BeatCount where  
    show Laghu = "| "  
    show Dhruta = "O "  
    show Anudhruta = "U "

type Thala = [BeatCount]

dhruva = [Laghu, Dhruta, Laghu, Laghu]
matya = [Laghu, Dhruta, Laghu]
rupaka = [Dhruta, Laghu]
jhampe = [Laghu, Dhruta, Anudhruta]
thriputa = [Laghu, Dhruta, Dhruta]
atta = [Laghu, Laghu, Dhruta, Dhruta]
eka = [Laghu]

calculateCount:: Thala -> JatiGati -> Int 
calculateCount [] _ = 0
calculateCount (x:xs) g = 
    case x of Laghu -> fromEnum g + calculateCount xs g
              Dhruta -> 2 + calculateCount xs g
              Anudhruta -> 1 + calculateCount xs g

