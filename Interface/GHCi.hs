module Interface.GHCi (NonEmpty(..), newAI) where

import Data.List.NonEmpty (NonEmpty)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import qualified Data.List.NonEmpty as L

import AI (Move, State, Ticket(..))
import qualified AI
import Interface.Utils

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

-- | Wrapper for 'printState'.
aiPrintState :: Show a => IORef (State a) -> IO ()
aiPrintState ref = printState putStr =<< readIORef ref
