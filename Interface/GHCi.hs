module Interface.GHCi (newAI) where

import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import qualified Data.Map as M
import Text.Show (shows, showString)

import AI (State, Move)
import Graph (Colour, Label(lweight))
import qualified AI

-- | GHCi interface to the AI.
--
-- @
-- (aiSuggest, aiPlan, aiReplan, aiDraw, aiDiscard, aiClaim, aiEnemyClaim, aiPrintState) <- newAI state
-- @
--
-- Then call the bound functions to interact with the game,
-- automatically updating the game state.
newAI :: (Enum a, Show a) => State a
  -> IO ( IO (Move a)
        , NonEmpty (a, a, Int) -> IO (NonEmpty (a, a, Int))
        , IO ()
        , [Maybe Colour] -> IO ()
        , [Maybe Colour] -> IO ()
        , a -> a -> Maybe Colour -> IO ()
        , a -> a -> Maybe Colour -> IO ()
        , IO ()
        )
newAI s = do
  ref <- newIORef s

  pure ( aiSuggest    ref
       , aiPlan       ref
       , aiReplan     ref
       , aiDraw       ref
       , aiDiscard    ref
       , aiClaim      ref
       , aiEnemyClaim ref
       , aiPrintState ref
       )

-- | Wrapper for 'AI.suggest'.
aiSuggest :: Enum a => IORef (State a) -> IO (Move a)
aiSuggest ref = AI.suggest <$> readIORef ref

-- | Wrapper for 'AI.planTickets'.
aiPlan :: Enum a => IORef (State a) -> NonEmpty (a, a, Int) -> IO (NonEmpty (a, a, Int))
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

-- | Wrapper for 'AI.draw'.
aiDraw :: IORef (State a) -> [Maybe Colour] -> IO ()
aiDraw ref colours = modifyIORef ref (AI.draw colours)

-- | Wrapper for 'AI.discard'.
aiDiscard :: IORef (State a) -> [Maybe Colour] -> IO ()
aiDiscard ref colours = modifyIORef ref (AI.discard colours)

-- | Wrapper for 'AI.claim'.
aiClaim :: Enum a => IORef (State a) -> a -> a -> Maybe Colour -> IO ()
aiClaim ref from to colour = modifyIORef ref (AI.claim from to colour)

-- | Wrapper for 'AI.enemyClaim'.
aiEnemyClaim :: Enum a => IORef (State a) -> a -> a -> Maybe Colour -> IO ()
aiEnemyClaim ref from to colour = modifyIORef ref (AI.claim from to colour)

-- | Print the state of the AI.
aiPrintState :: Show a => IORef (State a) -> IO ()
aiPrintState ref = do
  s <- readIORef ref
  putStrLn "=== AI State ===\n"

  case M.assocs (AI.hand s) of
    (c:cs) -> do
      putStr . showString "Hand: " . showCard c $ ""
      mapM_ (\c' -> putStr . showString ", " . showCard c' $ "") cs
    [] -> putStrLn "No hand!"

  putStrLn ""

  case AI.pendingTickets s of
    (t:ts) -> do
      putStrLn . showString "Tickets: " . showTicket t $ ""
      mapM_ (\t' -> putStrLn . showString "         " . showTicket t' $ "") ts
    [] -> putStrLn "No tickets!"

  putStrLn ""

  case AI.plan s of
    (p:ps) -> do
      putStrLn . showString "Plan: " . showPlanItem p $ ""
      mapM_ (\p' -> putStrLn . showString "      " . showPlanItem p' $ "") ps
    [] -> putStrLn "No plan!"

  where
    showCard (colour, num) = showColour colour . showAsideNum num
    showTicket (from, to, value) =
      shows from . showString " -> " . shows to . showAsideNum value
    showPlanItem (from, to, label) =
      shows from . showString " -> " . shows to . showAsideNum (lweight label)
    showColour Nothing  = showString "Locomotive"
    showColour (Just c) = shows c
    showAsideNum num = showString " (" . shows num . showString ")"
