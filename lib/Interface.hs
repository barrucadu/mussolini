{-# LANGUAGE LambdaCase #-}

module Interface (aiPlay) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf, nub)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as L
import qualified Data.Map as M
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT, outputStr, outputStrLn, setComplete)
import System.Console.Haskeline.Completion (completeWord, simpleCompletion)
import System.Console.Haskeline.MonadException (Exception, MonadException, SomeException, catch, throwIO)
import Text.Read (readMaybe)

import AI (State, Ticket(..), Move(..))
import Graph (Colour, Label(..))
import qualified Graph
import qualified AI

-- | An exception signalling that the terminal UI should terminate.
data Halt = Halt deriving Show
instance Exception Halt

-- | An interactive read-eval-print loop for playing as the AI.
aiPlay :: (Enum a, Bounded a, Ord a, Read a, Show a) => State a -> IO ()
aiPlay s0 = do
    cref <- newIORef []
    runInputT (settings cref) (playGame cref s0)
  where
    -- Haskeline doesn't have an easy way to change the available
    -- completions, so use an IORef.
    settings cref = setComplete (completeWord Nothing " \t" (completions cref)) defaultSettings
    completions cref str = map simpleCompletion . filter (isPrefixOf str) <$> readIORef cref

 -- | Play the game!
playGame :: (Enum a, Bounded a, Ord a, Read a, Show a)
  => IORef [String] -> State a -> InputT IO ()
playGame cref s0 = do
    s' <- initialise s0
    case s' of
      Just initialState -> do
        outputStrLn "\nEntering game loop..."
        help
        loop initialState `catchAll` const (pure ())
      Nothing -> pure ()
  where
    -- set up the initial game state.
    initialise s =
      prompt cref allColours "Cards in hand: "  readWords .>= \theHand  ->
      prompt cref allColours "Cards on table: " readWords .>= \theTable ->
      doDrawTickets cref . AI.draw theHand . AI.setCards theTable $ s

    -- a brief help message
    help = do
      outputStrLn "  s = suggest and play a move"
      outputStrLn "  c = claim   d = discard   e = enemy claim"
      outputStrLn "  T = set table cards   H = set hand cards   R = set remaining trains"
      outputStrLn "  p = print state   h = help   q = quit"

    -- game loop
    loop s = do
      outputStrLn ""
      s' <- prompt cref [] "ai> " Just >>= maybe (pure Nothing) (cmd s)
      case s' of
        Just newState
          | s /= newState -> do
            printDiff s newState
            outputStrLn ""
            accept <- confirm cref
            loop (if accept then newState else s)
        _ -> loop s

    -- process one command
    cmd s "s" = doMove cref s
    cmd s "c" = doClaim cref AI.claim s
    cmd s "d" = do
      c <- option cref "Discard: " (M.keys $ AI.hand s)
      pure ((\c' -> AI.discard [c'] s) <$> c)
    cmd s "e" = doClaim cref AI.enemyClaim s
    cmd s "T" = do
      cs <- prompt cref allColours "Cards on table: " readWords
      pure ((`AI.setCards` s) <$> cs)
    cmd s "H" = do
      cs <- prompt cref allColours "Cards on table: " readWords
      pure ((`AI.setHand` s) <$> cs)
    cmd s "R" = do
      t <- prompt cref [] "Remaining trains: " readMaybe
      pure ((\t' -> s { AI.remainingTrains = t' }) <$> t)
    cmd s "p" = do
      doPrintState s
      pure Nothing
    cmd _ "h" = do
      help
      pure Nothing
    cmd _ "q" = throwIO Halt
    cmd _ _ = pure Nothing


-------------------------------------------------------------------------------
-- Functions

-- | Suggest and perform a move.
doMove :: (Bounded a, Enum a, Read a, Show a, Ord a) => IORef [String] -> State a -> InputT IO (Maybe (State a))
doMove cref s = do
  let action = AI.suggest s
  outputStrLn (showMove action "\n")
  case action of
    AI.DrawLocomotiveCard ->
      prompt cref allColours "Replacement card: " readMaybe .>= \replacement ->
      pure . Just $ replaceDraw Graph.Special replacement s
    AI.DrawTickets -> doDrawTickets cref s
    AI.DrawCards (Just (c1, Just c2)) ->
      prompt cref allColours "Replacement cards: " (readWordsL 2) .>= \replacements ->
      pure . Just . replaceDraw c2 (replacements !! 1) $ replaceDraw c1 (head replacements) s
    AI.DrawCards (Just (c, _)) ->
      prompt cref allColours "Card from deck: "   readMaybe .>= \fromdeck ->
      prompt cref allColours "Replacement card: " readMaybe .>= \replacement ->
      pure . Just . AI.draw [fromdeck] $ replaceDraw c replacement s
    AI.DrawCards _ ->
      prompt cref allColours "Cards from deck: " (readWordsL 2) .>= \fromdeck ->
      pure . Just $ AI.draw fromdeck s
    (AI.ClaimRoute from to colour cards) ->
      pure . Just . AI.discard cards . AI.claim from to colour $ s

-- | Draw new tickets.
doDrawTickets :: (Bounded a, Enum a, Read a, Show a) => IORef [String] -> State a -> InputT IO (Maybe (State a))
doDrawTickets cref s =
  prompt cref (allPlaces s) "Tickets: " readTickets .>= \tickets -> do
  let (keep, plan) = AI.planTickets tickets s
  outputStr "\nKeep these tickets: "
  printList showTicket "none!" (L.toList keep)
  let s' = s { AI.pendingTickets = L.toList keep ++ AI.pendingTickets s
             , AI.plan = plan
             }
  pure (Just s')

-- | Register a claim.
doClaim :: (Bounded a, Enum a, Eq a, Read a, Show a)
  => IORef [String] -> (a -> a -> Colour -> State a -> State a) -> State a -> InputT IO (Maybe (State a))
doClaim cref claimf s =
  option cref "From: " (allPlaces' s) .>= \from ->
  option cref "To: " (Graph.neighbours from (AI.world s)) .>= \to ->
  case Graph.edgeFromTo from to (AI.world s) of
    Just lbl ->
      option cref "Colour: " (Graph.lcolour lbl) .>= \colour ->
      pure . Just $ claimf from to colour s
    Nothing -> pure Nothing

-- | Print out the AI state.
doPrintState :: Show a => State a -> InputT IO ()
doPrintState s = do
  outputStr . showString "Remaining trains: " . shows (AI.remainingTrains s) $ "\n"
  outputStrLn ""

  outputStr "Cards:\n"
  outputStr "\tIn hand:  " >> printList showCards "none!" (M.toList $ AI.hand    s)
  outputStr "\tOn table: " >> printList showCards "none!" (M.toList $ AI.ontable s)
  outputStrLn ""

  outputStr "Tickets:\n"
  outputStr "\tComplete: " >> printList showTicket "none!" (AI.completedTickets s)
  outputStr "\tPending:  " >> printList showTicket "none!" (AI.pendingTickets   s)
  outputStr "\tMissed:   " >> printList showTicket "none!" (AI.missedTickets    s)
  outputStrLn ""

  printPlan s


-------------------------------------------------------------------------------
-- (Utilities) Console

-- | Prompt for input, and parse it. Returns @Nothing@ if no input was given.
prompt :: IORef [String] -> [String] -> String -> (String -> Maybe a) -> InputT IO (Maybe a)
prompt cref completions msg f = liftIO (writeIORef cref completions) >> go where
  go = do
    input <- getInputLine msg
    case input of
      Just ""  -> pure Nothing
      Just str -> maybe (outputStrLn "Try again." >> go) (pure . Just) (f str)
      Nothing  -> pure Nothing

-- | Prompt for one of a set of options. If there is only one option,
-- this short-circuits and just returns that.
option :: (Eq a, Read a, Show a) => IORef [String] -> String -> [a] -> InputT IO (Maybe a)
option cref msg = go . nub where
  go [] = pure Nothing
  go [a] = do
    outputStrLn . showString msg . shows a $ " "
    pure (Just a)
  go as = prompt cref (map show as) msg $ \str -> case readMaybe str of
    Just a | a `elem` as -> Just a
    _ -> Nothing

-- | A confirmation message, where anything other than "y" or "yes"
-- (including empty input) is @False@.
confirm :: IORef [String] -> InputT IO Bool
confirm cref = do
  liftIO $ writeIORef cref ["y", "n"]
  input <- getInputLine "Accept? [y/N] "
  pure $ case input of
    Just str -> map toLower str `elem` ["y", "yes"]
    Nothing  -> False


-------------------------------------------------------------------------------
-- (Utilities) Pretty printing

-- | Show a card and a quantity.
showCards :: (Colour, Int) -> ShowS
showCards (colour, num) = shows colour . showAsideNum num

-- | Show a ticket, including its value.
showTicket :: Show a => Ticket a -> ShowS
showTicket (Ticket from to value) =
  shows from . showString " -> " . shows to . showAsideNum value

-- | Show an item in the plan, including its cost.
showPlanItem :: Show a => (a, a, Label) -> ShowS
showPlanItem (from, to, label) =
  showTicket $ Ticket from to (lweight label)

-- | Show a number in parentheses, after a space.
showAsideNum :: Int -> ShowS
showAsideNum num = showString " (" . shows num . showString ")"

-- | Descriptive text for a move.
showMove :: Show a => Move a -> ShowS
showMove DrawLocomotiveCard =
  showString "Draw a locomotive card from the table."
showMove (DrawCards (Just (c1, Just c2)))
  | c1 == c2 = showString "Draw two " . shows c1 . showString " cards from the table."
  | otherwise = showString "Draw a " . shows c1 . showString " and a " . shows c2 . showString " card from the table."
showMove (DrawCards (Just (c, Nothing))) =
  showString "Draw a " . shows c . showString " card from the table and one from the deck."
showMove (DrawCards Nothing) =
  showString "Draw two cards from the deck."
showMove (ClaimRoute from to colour cards) =
  showString "Build the " . shows colour . showString " route from " . shows from . showString " to " . shows to . showString " with " . showList cards . showString "."
showMove DrawTickets = showString "Draw new tickets."


-------------------------------------------------------------------------------
-- (Utilities) Parsing

-- | Parse a list.
readWords :: Read a => String -> Maybe [a]
readWords = traverse readMaybe . words

-- | Parse a list of the given length.
readWordsL :: Read a => Int -> String -> Maybe [a]
readWordsL len str = case readWords str of
  Just xs | length xs == len -> Just xs
  _ -> Nothing

-- | Parse tickets in the format \"From To Value\".
readTickets :: Read a => String -> Maybe (NonEmpty (Ticket a))
readTickets str = nonEmpty =<< go (words str) where
  go (from:to:value:rest) =
    let ticket = Ticket <$> readMaybe from <*> readMaybe to <*> readMaybe value
    in (:) <$> ticket <*> go rest
  go _ = Just []


-------------------------------------------------------------------------------
-- (Utilities) Output

-- | Print a list of items in a single line.
printList :: (a -> ShowS) -> String -> [a] -> InputT IO ()
printList _ none [] = outputStrLn none
printList f _ (x:xs) = do
  outputStr (f x "")
  mapM_ (\v -> outputStr . showString ", " . f v $ "") xs
  outputStrLn ""

-- | Print the plan, one route on each line, indented.
printPlan :: Show a => State a -> InputT IO ()
printPlan s = case AI.plan s of
  (p:ps) -> do
    outputStr . showString "Plan: " . showPlanItem p $ "\n"
    mapM_ (\p' -> outputStr . showString "      " . showPlanItem p' $ "\n") ps
  [] -> outputStrLn "No plan!"

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
    printList showTicket "none!" newTickets

  when (length (AI.missedTickets new) > length (AI.missedTickets old)) $ do
    let newTickets = filter (`notElem` AI.missedTickets old) (AI.missedTickets new)
    outputStr "Just missed: "
    printList showTicket "none!" newTickets

  when (AI.hand new /= AI.hand old) $ do
    outputStr "Cards in hand: "
    printList showCards "none!" (M.toList $ AI.hand new)

  when (AI.ontable new /= AI.ontable old) $ do
    outputStr "Cards on table: "
    printList showCards "none!" (M.toList $ AI.ontable new)

  when (AI.plan new /= AI.plan old) $
    printPlan new


-------------------------------------------------------------------------------
-- (Utilities) Misc

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

-- | Catch all exceptions.
catchAll :: MonadException m => m a -> (SomeException -> m a) -> m a
catchAll = catch

-- | All the colours as strings.
allColours :: [String]
allColours = map show allColours'

-- | All the places as strings.
allPlaces :: (Bounded a, Enum a, Show a) => proxy a -> [String]
allPlaces = map show . allPlaces'

-- | All the colours
allColours' :: [Colour]
allColours' = [minBound..maxBound]

-- | All the places.
allPlaces' :: (Bounded a, Enum a) => proxy a -> [a]
allPlaces' _ = [minBound..maxBound]
