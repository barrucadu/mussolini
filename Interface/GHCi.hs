module Interface.GHCi (newAI) where

import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import qualified Data.Map as M
import Text.Show (shows, showString)

import AI (Move, State, Ticket(..))
import Graph (Colour, Label(lweight))
import qualified AI

-- | GHCi interface to the AI.
--
-- @
-- (aiSuggest, aiPlan, aiReplan, aiDo, aiPrintState) <- newAI state
-- @
--
-- Then call the bound functions to interact with the game,
-- automatically updating the game state.
newAI :: (Enum a, Show a) => State a
  -> IO ( IO (Move a)
        , NonEmpty (Ticket a) -> IO (NonEmpty (Ticket a))
        , IO ()
        , (State a -> State a) -> IO ()
        , IO ()
        )
newAI s = do
  ref <- newIORef s

  pure ( aiSuggest    ref
       , aiPlan       ref
       , aiReplan     ref
       , modifyIORef  ref
       , aiPrintState ref
       )

-- | Wrapper for 'AI.suggest'.
aiSuggest :: Enum a => IORef (State a) -> IO (Move a)
aiSuggest ref = AI.suggest <$> readIORef ref

-- | Wrapper for 'AI.planTickets'.
aiPlan :: Enum a => IORef (State a) -> NonEmpty (Ticket a) -> IO (NonEmpty (Ticket a))
aiPlan ref ts = do
  s <- readIORef ref
  let (keep, plan) = AI.planTickets ts s
  writeIORef ref s { AI.pendingTickets = L.toList keep ++ AI.pendingTickets s
                   , AI.plan = plan
                   }
  pure keep

-- | Wrapper for 'AI.replanTickets'.
aiReplan :: Enum a => IORef (State a) -> IO ()
aiReplan ref = modifyIORef ref $ \s -> s { AI.plan = AI.replanTickets s }

-- | Print the state of the AI.
aiPrintState :: Show a => IORef (State a) -> IO ()
aiPrintState ref = do
  s <- readIORef ref
  putStrLn "=== AI State ===\n"

  putStrLn "Cards:"
  putStr "\tIn hand:  " >> printList showCard "none!" (M.toList $ AI.hand    s)
  putStr "\tOn table: " >> printList showCard "none!" (M.toList $ AI.ontable s)
  putStrLn ""

  putStrLn "Tickets:"
  putStr "\tComplete: " >> printList showTicket "none!" (AI.completedTickets s)
  putStr "\tPending:  " >> printList showTicket "none!" (AI.pendingTickets   s)
  putStr "\tMissed:   " >> printList showTicket "none!" (AI.missedTickets    s)
  putStrLn ""

  case AI.plan s of
    (p:ps) -> do
      putStrLn . showString "Plan: " . showPlanItem p $ ""
      mapM_ (\p' -> putStrLn . showString "      " . showPlanItem p' $ "") ps
    [] -> putStrLn "No plan!"

  where
    showCard (colour, num) = showColour colour . showAsideNum num
    showTicket (Ticket from to value) =
      shows from . showString " -> " . shows to . showAsideNum value
    showPlanItem (from, to, label) =
      shows from . showString " -> " . shows to . showAsideNum (lweight label)
    showColour Nothing  = showString "Locomotive"
    showColour (Just c) = shows c
    showAsideNum num = showString " (" . shows num . showString ")"

    printList _ none [] = putStrLn none
    printList f _ (x:xs) = do
      putStr (f x "")
      mapM_ (\v -> putStr . showString ", " . f v $ "") xs
      putStr "\n"
