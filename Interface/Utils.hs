module Interface.Utils where

import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as M
import Text.Read (readMaybe)

import AI (State, Ticket(..), Move(..))
import Graph (Colour(..), Label(..))
import qualified AI

-------------------------------------------------------------------------------
-- Pretty printing

showCard :: (Colour, Int) -> ShowS
showCard (colour, num) = shows colour . showAsideNum num

showTicket :: Show a => Ticket a -> ShowS
showTicket (Ticket from to value) =
  shows from . showString " -> " . shows to . showAsideNum value

showPlanItem :: Show a => (a, a, Label) -> ShowS
showPlanItem (from, to, label) =
  shows from . showString " -> " . shows to . showAsideNum (lweight label)

showAsideNum :: Int -> ShowS
showAsideNum num = showString " (" . shows num . showString ")"

showMove :: Show a => Move a -> ShowS
showMove DrawLocomotiveCard =
  showString "Draw a locomotive card from the table."
showMove (DrawCards (Just c1) (Just c2))
  | c1 == c2 = showString "Draw two " . shows c1 . showString " cards from the table."
  | otherwise = showString "Draw a " . shows c1 . showString " and a " . shows c2 . showString " card from the table."
showMove (DrawCards (Just c) _) =
  showString "Draw a " . shows c . showString " card from the table and one from the deck."
showMove (DrawCards _ _) =
  showString "Draw two cards from the deck."
showMove (ClaimRoute from to colour cards) =
  showString "Build the " . shows colour . showString " route from " . shows from . showString " to " . shows to . showString " with " . showList cards . showString "."
showMove DrawTickets = showString "Draw new tickets."


-------------------------------------------------------------------------------
-- Parsing

readWords :: Read a => String -> Maybe [a]
readWords = traverse readMaybe . words

readWordsL :: Read a => Int -> String -> Maybe [a]
readWordsL len str = case readWords str of
  Just xs | length xs == len -> Just xs
  _ -> Nothing

readTickets :: Read a => String -> Maybe (NonEmpty (Ticket a))
readTickets str = nonEmpty =<< go (words str) where
  go (from:to:value:rest) =
    let ticket = Ticket <$> readMaybe from <*> readMaybe to <*> readMaybe value
    in (:) <$> ticket <*> go rest
  go _ = Just []


-------------------------------------------------------------------------------
-- Output

printList :: Monad m => (String -> m ()) -> (a -> ShowS) -> String -> [a] -> m ()
printList puts _ none [] = puts none >> puts "\n"
printList puts f _ (x:xs) = do
  puts (f x "")
  mapM_ (\v -> puts . showString ", " . f v $ "") xs
  puts "\n"

printPlan :: (Show a, Monad m) => (String -> m ()) -> State a -> m ()
printPlan puts s = case AI.plan s of
  (p:ps) -> do
    puts . showString "Plan: " . showPlanItem p $ "\n"
    mapM_ (\p' -> puts . showString "      " . showPlanItem p' $ "\n") ps
  [] -> puts "No plan!\n"

printState :: (Show a, Monad m) => (String -> m ()) -> State a -> m ()
printState puts s = do
  puts . showString "Remaining trains: " . shows (AI.remainingTrains s) $ "\n"
  puts "\n"

  puts "Cards:\n"
  puts "\tIn hand:  " >> printList puts showCard "none!" (M.toList $ AI.hand    s)
  puts "\tOn table: " >> printList puts showCard "none!" (M.toList $ AI.ontable s)
  puts "\n"

  puts "Tickets:\n"
  puts "\tComplete: " >> printList puts showTicket "none!" (AI.completedTickets s)
  puts "\tPending:  " >> printList puts showTicket "none!" (AI.pendingTickets   s)
  puts "\tMissed:   " >> printList puts showTicket "none!" (AI.missedTickets    s)
  puts "\n"

  printPlan puts s
