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
    initialise s = do
      theHand <- prompt "Cards in hand: " readWords
      case theHand of
        Just hand -> do
          theTable <- prompt "Cards on table: " readWords
          case theTable of
            Just table ->
              doDrawTickets . AI.draw hand . AI.setCards table $ s
            Nothing -> pure Nothing
        Nothing -> pure Nothing

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
doDrawSpecial s = do
  mreplacement <- prompt "Replacement card: " readMaybe
  pure $ do
    replacement <- mreplacement
    pure $ replaceDraw Special replacement s

-- | Draw a pair of cards
doDraw :: Maybe Colour -> Maybe Colour -> State a -> InputT IO (Maybe (State a))
doDraw (Just c1) (Just c2) s = do
  mreplacements <- prompt "Replacement cards: " (readWordsL 2)
  pure $ do
    replacements <- mreplacements
    pure . replaceDraw c2 (replacements !! 1) . replaceDraw c1 (head replacements) $ s
doDraw (Just c) _ s = do
  mfromdeck  <- prompt "Card from deck: " readMaybe
  case mfromdeck of
    Just fromdeck -> do
      mreplacement <- prompt "Replacement card: " readMaybe
      pure $ do
        replacement <- mreplacement
        pure . AI.draw [fromdeck] . replaceDraw c replacement $ s
    Nothing -> pure Nothing
doDraw _ _ s = do
  mfromdeck <- prompt "Cards from deck: " (readWordsL 2)
  pure $ do
    fromdeck <- mfromdeck
    pure $ AI.draw fromdeck s

-- | Claim a route.
doClaim :: Enum a => a -> a -> Colour -> [Colour] -> State a -> State a
doClaim from to colour cards =
  AI.discard cards . AI.claim from to colour

-- | Draw tickets.
doDrawTickets :: (Enum a, Read a, Show a) => State a -> InputT IO (Maybe (State a))
doDrawTickets s = do
  mtickets <- prompt "Tickets: " readTickets
  case mtickets of
    Just tickets -> do
      let (keep, plan) = AI.planTickets tickets s
      outputStr "\nKeep these tickets: "
      printList outputStr showTicket "none!" (L.toList keep)
      let s' = s { AI.pendingTickets = L.toList keep ++ AI.pendingTickets s
                 , AI.plan = plan
                 }
      pure (Just s')
    Nothing -> pure Nothing

-- | Register an enemy claim.
doEnemyClaim :: (Enum a, Read a) => State a -> InputT IO (Maybe (State a))
doEnemyClaim s = do
  mfrom <- prompt "From:" readMaybe
  case mfrom of
    Just from -> do
      mto <- prompt "To: " readMaybe
      case mto of
        Just to -> do
          mcolour <- prompt "Colour: " readMaybe
          pure $ do
            colour <- mcolour
            pure $ AI.enemyClaim from to colour s
        Nothing -> pure Nothing
    Nothing -> pure Nothing

-- | Set the visible cards.
doSetCards :: State a -> InputT IO (Maybe (State a))
doSetCards s = do
  mtable <- prompt "Cards on table: " readWords
  pure $ do
    table <- mtable
    pure $ AI.setCards table s

-------------------------------------------------------------------------------
-- Utilities

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
