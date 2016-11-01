{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interface.Haskeline (aiPlay) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as L
import qualified Data.Map as M
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT, outputStr, outputStrLn, setComplete)
import System.Console.Haskeline.Completion (completeWord, simpleCompletion)
import Text.Read (readMaybe)

import AI (State)
import Graph (Colour(Special))
import qualified AI
import Interface.Utils

-- | An interactive read-eval-print loop for playing as the AI.
aiPlay :: forall a. (Enum a, Bounded a, Eq a, Read a, Show a) => State a -> IO ()
aiPlay s0 = runInputT settings $ do
    s' <- initialise s0

    outputStrLn "\nEntering game loop..."
    help
    loop s'
  where
    -- the haskeline settings: completion for place names.
    settings = setComplete (completeWord Nothing " \t" completePlace) defaultSettings
    completePlace str = pure . map simpleCompletion . filter (isPrefixOf str) $ map show allPlaces ++ map show allColours
    allPlaces = [minBound..maxBound] :: [a]
    allColours = [minBound..maxBound] :: [Colour]

    -- set up the initial game state.
    initialise s = do
      theHand  <- prompt "Cards in hand: "  readWords
      theTable <- prompt "Cards on table: " readWords

      doDrawTickets . AI.draw theHand . AI.setCards theTable $ s

    -- a brief help message
    help = do
      outputStrLn "  s = suggest and play a move"
      outputStrLn "  e = enemy claim   v = set visible cards"
      outputStrLn "  p = print state   h = help     q = quit"

    -- game loop
    loop s = do
      outputStrLn ""
      (s', continue) <- prompt "ai> " Just >>= \case
        "s" -> do
          let action = AI.suggest s
          outputStrLn (showMove action "\n")
          s' <- case action of
            AI.DrawLocomotiveCard -> doDrawSpecial s
            (AI.DrawCards c1 c2) -> doDraw c1 c2 s
            (AI.ClaimRoute from to colour cards) -> pure $ doClaim from to colour cards s
            AI.DrawTickets -> doDrawTickets s
          pure (s', True)
        "e" -> do
          s' <- doEnemyClaim s
          pure (s', True)
        "v" -> do
          s' <- doSetCards s
          pure (s', True)
        "p" -> do
          printState outputStr s
          pure (s, True)
        "h" -> do
          help
          pure (s, True)
        "q" -> pure (s, False)
        _   -> pure (s, True)

      when continue $ do
        printDiff s s'
        newState <- if s /= s'
          then do
            outputStrLn ""
            accept <- prompt "Accept? [y/n] " (Just . map toLower)
            pure $ if accept `elem` ["accept", "y", "yes"] then s' else s
          else pure s
        loop newState

-------------------------------------------------------------------------------
-- Functions

-- | Draw a locomotive
doDrawSpecial :: State a -> InputT IO (State a)
doDrawSpecial s = do
  replacement <- prompt "Replacement card: " readMaybe
  pure $ replaceDraw Special replacement s

-- | Draw a pair of cards
doDraw :: Maybe Colour -> Maybe Colour -> State a -> InputT IO (State a)
doDraw (Just c1) (Just c2) s = do
  replacements <- prompt "Replacement cards: " (readWordsL 2)
  pure . replaceDraw c2 (replacements !! 1) . replaceDraw c1 (head replacements) $ s
doDraw (Just c) _ s = do
  fromdeck    <- prompt "Card from deck:   " readMaybe
  replacement <- prompt "Replacement card: " readMaybe
  pure . AI.draw [fromdeck] . replaceDraw c replacement $ s
doDraw _ _ s = do
  fromdeck <- prompt "Cards from deck: " (readWordsL 2)
  pure $ AI.draw fromdeck s

-- | Claim a route.
doClaim :: Enum a => a -> a -> Colour -> [Colour] -> State a -> State a
doClaim from to colour cards =
  AI.discard cards . AI.claim from to colour

-- | Draw tickets.
doDrawTickets :: (Enum a, Read a, Show a) => State a -> InputT IO (State a)
doDrawTickets s = do
  theTickets <- prompt "Tickets: " readTickets
  let (keep, plan) = AI.planTickets theTickets s
  outputStr "\nKeep these tickets: "
  printList outputStr showTicket "none!" (L.toList keep)
  let s' = s { AI.pendingTickets = L.toList keep ++ AI.pendingTickets s
             , AI.plan = plan
             }
  pure s'

-- | Register an enemy claim.
doEnemyClaim :: (Enum a, Read a) => State a -> InputT IO (State a)
doEnemyClaim s = do
  from   <- prompt "From:   " readMaybe
  to     <- prompt "To:     " readMaybe
  colour <- prompt "Colour: " readMaybe

  pure $ AI.enemyClaim from to colour s

-- | Set the visible cards.
doSetCards :: State a -> InputT IO (State a)
doSetCards s = do
  theTable <- prompt "Cards on table: " readWords
  pure $ AI.setCards theTable s

-------------------------------------------------------------------------------
-- Utilities

-- | Draw a card with replacement.
replaceDraw :: Colour -> Colour -> State a -> State a
replaceDraw original replacement s =
  let s' = AI.draw [original] s
      onTable  = M.update (\i -> if i > 1 then Just (i-1) else Nothing) original (AI.ontable s)
      onTable' = M.alter (\case Just i -> Just (i+1); Nothing -> Just 1) replacement onTable
  in s' { AI.ontable = onTable' }

-- | Prompt for input, and parse it.
prompt :: String -> (String -> Maybe a) -> InputT IO a
prompt msg f = go where
  go = do
    input <- getInputLine msg
    case input of
      Just str -> maybe (outputStrLn "Try again." >> go) pure (f str)
      Nothing  -> go

-- | Print the difference between two states.
--
-- Changes to the pending tickets displayed, as the user explicitly
-- enters that information.
printDiff :: (Eq a, Show a) => State a -> State a -> InputT IO ()
printDiff old new = do
  when (AI.remainingTrains new /= AI.remainingTrains old) $
    outputStrLn $ "Remaining trains: " ++ show (AI.remainingTrains new)

  when (length (AI.completedTickets new) > length (AI.completedTickets old)) $ do
    let newTickets = filter (`notElem` AI.completedTickets old) (AI.completedTickets new)
    outputStr "Just completed: "
    printList outputStr showTicket "none!" newTickets

  when (length (AI.missedTickets new) > length (AI.missedTickets old)) $ do
    let newTickets = filter (`notElem` AI.missedTickets old) (AI.missedTickets new)
    outputStr "Just missed: "
    printList outputStr showTicket "none!" newTickets

  when (AI.hand new /= AI.hand old) $ do
    outputStr "Cards in hand: "
    printList outputStr showCard "none!" (M.toList $ AI.hand new)

  when (AI.ontable new /= AI.ontable old) $ do
    outputStr "Cards on table: "
    printList outputStr showCard "none!" (M.toList $ AI.hand new)

  when (AI.plan new /= AI.plan old) $
    printPlan outputStr new
