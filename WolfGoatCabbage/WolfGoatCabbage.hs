import Data.List as List
import Data.Set as Set

data Passenger = Wolf | Goat | Cabbage deriving ( Eq, Ord, Show )

data FarmerPosition = FarmerLeft | FarmerRight deriving ( Show )

type Position = ( [ Passenger ], [ Passenger ], FarmerPosition )

type Journey = [ Position ]

-- main : output the solution to the puzzle
main = do
    mapM_ ( putStrLn . show ) $ findSolution [ start ]

-- start : the journey configuration to start the puzzle from
start :: Journey
start = [ ( [ Wolf, Goat, Cabbage ], [ ], FarmerLeft ) ]

-- findSolution journeys : find one of the 'journeys' that solves the puzzle
findSolution :: [ Journey ] -> Journey
findSolution js = case find ( isFinish . head ) result of
    Just finish -> reverse finish
    Nothing -> findSolution result
    where result = js >>= journeysFrom

-- isFinish position : 'True' if 'position' is the final puzzle position
isFinish :: Position -> Bool
isFinish ( [ ], _, FarmerRight ) = True
isFinish _                       = False

-- journeysFrom journey : returns the possible journeys that can be made from
--                        'journey'
journeysFrom :: Journey -> [ Journey ]
journeysFrom js = Prelude.map ( \x -> x : js ) . movesFrom . head $ js

-- movesFrom position : returns the possible moves that can be made from
--                      'position'
movesFrom :: Position -> [ Position ]
movesFrom ( as, bs, farmer ) = case farmer of
    FarmerLeft -> ( as, bs, FarmerRight ) : [ ( List.delete a as, a : bs, FarmerRight ) | a <- as, as `notTroubleWithout` a ]
    FarmerRight -> ( as, bs, FarmerLeft ) : [ ( b : as, List.delete b bs, FarmerLeft ) | b <- bs, bs `notTroubleWithout` b ]

-- passengers `notTroubleWithout` passenger : 'True' if 'passengers' can be left
--                                            alone without 'passenger'
notTroubleWithout :: [ Passenger ] -> Passenger -> Bool
notTroubleWithout xs x = not . trouble $ List.delete x xs

-- trouble passengers : 'True' if leaving 'passengers' alone together is trouble
trouble :: [ Passenger ] -> Bool
trouble ps = set == wolfGoat || set == goatCabbage
    where set = Set.fromList ps

-- wolfGoat : a troublesome set of passengers
wolfGoat :: Set Passenger
wolfGoat = Set.fromList [ Wolf, Goat ]

-- goatCabbage : a troublesome set of passengers
goatCabbage :: Set Passenger
goatCabbage = Set.fromList [ Goat, Cabbage ]
