module Util where

-- | The number of points gained by buying a route of this length.
routeScore :: Num n => Int -> Maybe n
routeScore 1 = Just 1
routeScore 2 = Just 2
routeScore 3 = Just 4
routeScore 4 = Just 7
routeScore 6 = Just 13
routeScore 8 = Just 21
routeScore _ = Nothing
