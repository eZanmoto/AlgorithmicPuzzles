import Data.List as List
import Data.Set as Set

data Passenger = Wolf | Goat | Cabbage deriving ( Eq, Ord, Show )

data FarmerPosition = FarmerLeft | FarmerRight deriving ( Show )

type Position = ( [ Passenger ], [ Passenger ], FarmerPosition )

type Journey = [ Position ]

main = do
    mapM_ ( putStrLn . show ) $ findSolution [ start ]

start :: Journey
start = [ ( [ Wolf, Goat, Cabbage ], [ ], FarmerLeft ) ]

findSolution :: [ Journey ] -> Journey
findSolution js = case find ( isFinish . head ) result of
    Just finish -> reverse finish
    Nothing -> findSolution result
    where result = js >>= journeysFromJourney

isFinish :: Position -> Bool
isFinish ( [ ], _, FarmerRight ) = True
isFinish _                       = False

journeysFromJourney :: Journey -> [ Journey ]
journeysFromJourney js = Prelude.map ( \x -> x : js ) . movesFrom . head $ js

movesFrom :: Position -> [ Position ]
movesFrom ( as, bs, farmer ) = case farmer of
    FarmerLeft -> ( as, bs, FarmerRight ) : [ ( List.delete a as, a : bs, FarmerRight ) | a <- as, as `notTroubleWithout` a ]
    FarmerRight -> ( as, bs, FarmerLeft ) : [ ( b : as, List.delete b bs, FarmerLeft ) | b <- bs, bs `notTroubleWithout` b ]

notTroubleWithout :: [ Passenger ] -> Passenger -> Bool
notTroubleWithout xs x = not . trouble $ List.delete x xs

trouble :: [ Passenger ] -> Bool
trouble ps = set == wolfGoat || set == goatCabbage
    where set = Set.fromList ps

wolfGoat :: Set Passenger
wolfGoat = Set.fromList [ Wolf, Goat ]

goatCabbage :: Set Passenger
goatCabbage = Set.fromList [ Goat, Cabbage ]
