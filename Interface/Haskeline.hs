{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interface.Haskeline (aiPlay) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (isPrefixOf, nub)
import qualified Data.List.NonEmpty as L
import qualified Data.Map as M
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT, outputStr, outputStrLn, setComplete)
import System.Console.Haskeline.Completion (completeWord, simpleCompletion)
import Text.Read (readMaybe)

import AI (State)
import Graph (Colour(Special))
import qualified Graph
import qualified AI
import Interface.Utils

-- | An interactive read-eval-print loop for playing as the AI.
aiPlay :: forall a. (Enum a, Bounded a, Eq a, Read a, Show a) => State a -> IO ()
aiPlay s0 = runInputT settings $ do
    s' <- initialise s0
    case s' of
      Just initialState -> do
        outputStrLn "\nEntering game loop..."
        help
        loop initialState
      Nothing -> pure ()
  where
    -- the haskeline settings: completion for place names.
    settings = setComplete (completeWord Nothing " \t" completePlace) defaultSettings
    completePlace str = pure . map simpleCompletion . filter (isPrefixOf str) $ map show allPlaces ++ map show allColours
    allPlaces = [minBound..maxBound] :: [a]
    allColours = [minBound..maxBound] :: [Colour]

    -- set up the initial game state.
    initialise s =
      prompt "Cards in hand: "  readWords .>= \theHand  ->
      prompt "Cards on table: " readWords .>= \theTable ->
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
        Just "s" -> do
          let action = AI.suggest s
          outputStrLn (showMove action "\n")
          s' <- case action of
            AI.DrawLocomotiveCard -> doDrawSpecial s
            (AI.DrawCards c1 c2) -> doDraw c1 c2 s
            (AI.ClaimRoute from to colour cards) -> pure . Just $ doClaim from to colour cards s
            AI.DrawTickets -> doDrawTickets s
          pure (s', True)
        Just "e" -> do
          s' <- doEnemyClaim s
          pure (s', True)
        Just "v" -> do
          s' <- doSetCards s
          pure (s', True)
        Just "p" -> do
          printState outputStr s
          pure (Nothing, True)
        Just "h" -> do
          help
          pure (Nothing, True)
        Just "q" -> pure (Nothing, False)
        _   -> pure (Nothing, True)

      when continue $
        case s' of
          Just newState
            | s /= newState -> do
              printDiff s newState
              outputStrLn ""
              accept <- confirm
              let realNewState = if accept then newState else s
              loop realNewState
            | otherwise -> loop s
          Nothing -> loop s

-------------------------------------------------------------------------------
-- Functions

-- | Draw a locomotive
doDrawSpecial :: State a -> InputT IO (Maybe (State a))
doDrawSpecial s =
  prompt "Replacement card: " readMaybe .>= \replacement ->
  pure . Just $ replaceDraw Special replacement s

-- | Draw a pair of cards
doDraw :: Maybe Colour -> Maybe Colour -> State a -> InputT IO (Maybe (State a))
doDraw (Just c1) (Just c2) s =
  prompt "Replacement cards: " (readWordsL 2) .>= \replacements ->
  pure . Just . replaceDraw c2 (replacements !! 1) $ replaceDraw c1 (head replacements) s
doDraw (Just c) _ s =
  prompt "Card from deck: "   readMaybe .>= \fromdeck ->
  prompt "Replacement card: " readMaybe .>= \replacement ->
  pure . Just . AI.draw [fromdeck] $ replaceDraw c replacement s
doDraw _ _ s =
  prompt "Cards from deck: " (readWordsL 2) .>= \fromdeck ->
  pure . Just $ AI.draw fromdeck s

-- | Claim a route.
doClaim :: Enum a => a -> a -> Colour -> [Colour] -> State a -> State a
doClaim from to colour cards =
  AI.discard cards . AI.claim from to colour

-- | Draw tickets.
doDrawTickets :: (Enum a, Read a, Show a) => State a -> InputT IO (Maybe (State a))
doDrawTickets s =
  prompt "Tickets: " readTickets .>= \tickets -> do
    let (keep, plan) = AI.planTickets tickets s
    outputStr "\nKeep these tickets: "
    printList outputStr showTicket "none!" (L.toList keep)
    let s' = s { AI.pendingTickets = L.toList keep ++ AI.pendingTickets s
               , AI.plan = plan
               }
    pure (Just s')

-- | Register an enemy claim.
doEnemyClaim :: (Enum a, Read a) => State a -> InputT IO (Maybe (State a))
doEnemyClaim s =
  prompt "From: " readMaybe .>= \from ->
  prompt "To: "   readMaybe .>= \to ->
  case Graph.edgeFromTo from to (AI.world s) of
    Just lbl ->
      option "Colour: " (Graph.lcolour lbl) .>= \colour ->
      pure . Just $ AI.enemyClaim from to colour s
    Nothing -> pure Nothing

-- | Set the visible cards.
doSetCards :: State a -> InputT IO (Maybe (State a))
doSetCards s =
  prompt "Cards on table: " readWords .>= \table ->
  pure . Just $ AI.setCards table s

-------------------------------------------------------------------------------
-- Utilities

-- | Like '>>=' but nicely dealing with @Maybe@-values.
(.>=) :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
mma .>= mmf = mma >>= maybe (pure Nothing) mmf

-- | Draw a card with replacement.
replaceDraw :: Colour -> Colour -> State a -> State a
replaceDraw original replacement s =
  let s' = AI.draw [original] s
      onTable  = M.update (\i -> if i > 1 then Just (i-1) else Nothing) original (AI.ontable s)
      onTable' = M.alter (\case Just i -> Just (i+1); Nothing -> Just 1) replacement onTable
  in s' { AI.ontable = onTable' }

-- | Prompt for input, and parse it. Returns @Nothing@ if no input was given.
prompt :: String -> (String -> Maybe a) -> InputT IO (Maybe a)
prompt msg f = go where
  go = do
    input <- getInputLine msg
    case input of
      Just ""  -> pure Nothing
      Just str -> maybe (outputStrLn "Try again." >> go) (pure . Just) (f str)
      Nothing  -> pure Nothing

-- | Prompt for one of a set of options. If there is only one option,
-- this short-circuits and just returns that.
option :: (Eq a, Read a, Show a) => String -> [a] -> InputT IO (Maybe a)
option msg = go . nub where
  go [] = pure Nothing
  go [a] = pure (Just a)
  go as = prompt msg' f' where
    msg' = showString msg . showList as $ " "
    f' str = case readMaybe str of
      Just a | a `elem` as -> Just a
      _ -> Nothing

-- | A confirmation message, where anything other than "y" or "yes"
-- (including empty input) is @False@.
confirm :: InputT IO Bool
confirm = do
  input <- getInputLine "Accept? [y/N] "
  pure $ case input of
    Just str -> map toLower str `elem` ["y", "yes"]
    Nothing  -> False

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
    printList outputStr showCard "none!" (M.toList $ AI.ontable new)

  when (AI.plan new /= AI.plan old) $
    printPlan outputStr new
