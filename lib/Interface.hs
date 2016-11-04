{-# LANGUAGE LambdaCase #-}

module Interface (aiPlay) where

import Control.Arrow (first)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Data.Char (toLower)
import Data.Foldable (for_)
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

-- | An interactive read-eval-print loop for playing as the AI.
aiPlay :: (Enum a, Bounded a, Ord a, Read a, Show a) => State a -> IO ()
aiPlay s0 = do
    cref <- newIORef []
    flip runReaderT cref . runInputT (settings cref) $ playGame s0
  where
    -- Haskeline doesn't have an easy way to change the available
    -- completions, so use an IORef.
    settings cref = setComplete (completeWord Nothing " \t" (completions cref)) defaultSettings
    completions cref str = map simpleCompletion . filter (isPrefixOf str) <$> liftIO (readIORef cref)


-------------------------------------------------------------------------------
-- Play the game!

-- | The monad the terminal UI runs inside.
type UI = InputT (ReaderT (IORef [String]) IO)

-- | An exception signalling that the terminal UI should terminate.
data Halt = Halt deriving Show
instance Exception Halt

-- | Play the game!
playGame :: (Enum a, Bounded a, Ord a, Read a, Show a) => State a -> UI ()
playGame s0 = do
  s' <- initialise s0
  case s' of
    Just initialState -> do
      outputStrLn "\nEntering game loop..."
      help
      gameloop initialState `catchAll` const (pure ())
    Nothing -> pure ()

-- | Set up the initial game state.
initialise :: (Bounded a, Enum a, Eq a, Read a, Show a) => State a -> UI (Maybe (State a))
initialise s0 =
  prompt allColours "Enemies: " readWords        .>= \theEnemies ->
  prompt allColours "Cards in hand: "  readWords .>= \theHand    ->
  prompt allColours "Cards on table: " readWords .>= \theTable   ->
  doDrawTickets . AI.draw theHand . AI.setCards theTable . AI.setEnemies theEnemies $ s0

-- | A brief help message.
help :: UI ()
help = do
  outputStrLn "  s = suggest and play a move"
  outputStrLn "  c = claim   d = discard   e = enemy claim"
  outputStrLn "  T = set table cards   H = set hand cards   R = set remaining trains"
  outputStrLn "  p = print state   h = help   q = quit"

-- | Game loop.
gameloop :: (Bounded a, Enum a, Read a, Show a, Ord a) => State a -> UI ()
gameloop s = do
    outputStrLn ""
    s' <- prompt [] "ai> " Just >>= maybe (pure Nothing) cmd
    case s' of
      Just newState
        | s /= newState -> do
          outputStrLn ""
          printDiff s newState
          outputStrLn ""
          accept <- confirm
          gameloop (if accept then newState else s)
      _ -> gameloop s
  where
    -- process one command
    cmd "s" = doMove s
    cmd "c" = doClaim AI.claim s
    cmd "d" = do
      c <- option "Discard: " (M.keys $ AI.hand s)
      pure ((\c' -> AI.discard [c'] s) <$> c)
    cmd "e" =
      option "Enemy: " (M.keys (AI.enemies s)) .>= \who ->
      doClaim (AI.enemyClaim who) s
    cmd "T" = do
      cs <- prompt allColours "Cards on table: " readWords
      pure ((`AI.setCards` s) <$> cs)
    cmd "H" = do
      cs <- prompt allColours "Cards in hand: " readWords
      pure ((`AI.setHand` s) <$> cs)
    cmd "R" = do
      t <- prompt [] "Remaining trains: " readMaybe
      pure ((\t' -> s { AI.trains = AI.initialTrains s - t' }) <$> t)
    cmd "p" = do
      doPrintState s
      pure Nothing
    cmd "h" = do
      help
      pure Nothing
    cmd "q" = throwIO Halt
    cmd _ = pure Nothing


-------------------------------------------------------------------------------
-- Functions

-- | Suggest and perform a move.
doMove :: (Bounded a, Enum a, Read a, Show a, Ord a) => State a -> UI (Maybe (State a))
doMove s = do
  let action = AI.suggest s
  outputStrLn (showMove action "")
  case action of
    AI.DrawLocomotiveCard ->
      outputStrLn "" >>
      prompt allColours "Replacement card: " readMaybe .>= \replacement ->
      pure . Just $ replaceDraw Graph.Special replacement s
    AI.DrawTickets -> outputStrLn "" >> doDrawTickets s
    AI.DrawCards (Just (c1, Just c2)) ->
      outputStrLn "" >>
      prompt allColours "Replacement cards: " (readWordsL 2) .>= \replacements ->
      pure . Just . replaceDraw c2 (replacements !! 1) $ replaceDraw c1 (head replacements) s
    AI.DrawCards (Just (c, _)) ->
      outputStrLn "" >>
      prompt allColours "Card from deck: "   readMaybe .>= \fromdeck ->
      prompt allColours "Replacement card: " readMaybe .>= \replacement ->
      pure . Just . AI.draw [fromdeck] $ replaceDraw c replacement s
    AI.DrawCards _ ->
      outputStrLn "" >>
      prompt allColours "Cards from deck: " (readWordsL 2) .>= \fromdeck ->
      pure . Just $ AI.draw fromdeck s
    (AI.ClaimRoute from to colour cards) ->
      pure . Just . AI.discard cards . AI.claim from to colour $ s

-- | Draw new tickets.
doDrawTickets :: (Bounded a, Enum a, Eq a, Read a, Show a) => State a -> UI (Maybe (State a))
doDrawTickets s =
  prompt (allPlaces s) "Tickets: " readTickets .>= \tickets -> do
  let (keep, plan) = first L.toList (AI.planTickets tickets s)
  let discard = filter (`notElem` keep) (L.toList tickets)
  outputStr "\nKeep these tickets: "
  printList showTicket "none!" keep
  let s' = s { AI.pendingTickets   = keep    ++ AI.pendingTickets s
             , AI.discardedTickets = discard ++ AI.discardedTickets s
             , AI.plan = plan
             }
  pure (Just s')

-- | Register a claim.
doClaim :: (Bounded a, Enum a, Eq a, Read a, Show a)
  => (a -> a -> Colour -> State a -> State a) -> State a -> UI (Maybe (State a))
doClaim claimf s =
  option "From: " (allPlaces' s) .>= \from ->
  option "To: " (Graph.neighbours from (AI.world s)) .>= \to ->
  case Graph.edgeFromTo from to (AI.world s) of
    Just lbl ->
      option "Colour: " (Graph.lcolour lbl) .>= \colour ->
      pure . Just $ claimf from to colour s
    Nothing -> pure Nothing

-- | Print out the AI state.
doPrintState :: (Enum a, Show a) => State a -> UI ()
doPrintState s = do
  outputStr . showString "Score: " . shows (AI.score s) . showTicketScore s $ "\n"
  outputStr . showString "Remaining trains: " . shows (AI.remainingTrains s) $ "\n"
  outputStr . showString "Min. remaining turns: " . shows (AI.remainingTurns s) $ "\n"
  outputStrLn ""

  outputStrLn "Enemies:"
  for_ (M.keys $ AI.enemies s) $ \who ->
    outputStr . showString "\t" . shows who . showString ": " . showEnemy who s $ "\n"

  outputStrLn "Cards:"
  outputStr "\tIn hand: "  >> printList showCards "none!" (M.toList $ AI.hand    s)
  outputStr "\tOn table: " >> printList showCards "none!" (M.toList $ AI.ontable s)
  outputStrLn ""

  outputStrLn "Tickets:"
  outputStr "\tComplete: "  >> printList showTicket "none!" (AI.completedTickets s)
  outputStr "\tPending: "   >> printList showTicket "none!" (AI.pendingTickets   s)
  outputStr "\tMissed: "    >> printList showTicket "none!" (AI.missedTickets    s)
  outputStr "\tDiscarded: " >> printList showTicket "none!" (AI.discardedTickets s)
  outputStrLn ""

  printPlan s


-------------------------------------------------------------------------------
-- (Utilities) Console

-- | Prompt for input, and parse it. Returns @Nothing@ if no input was given.
prompt :: [String] -> String -> (String -> Maybe a) -> UI (Maybe a)
prompt completions msg f = setCompletions completions >> go where
  go = do
    input <- getInputLine msg
    case input of
      Just ""  -> pure Nothing
      Just str -> maybe (outputStrLn "Try again." >> go) (pure . Just) (f str)
      Nothing  -> pure Nothing

-- | Prompt for one of a set of options. If there is only one option,
-- this short-circuits and just returns that.
option :: (Eq a, Read a, Show a) => String -> [a] -> UI (Maybe a)
option msg = go . nub where
  go [] = pure Nothing
  go [a] = do
    outputStrLn . showString msg . shows a $ " "
    pure (Just a)
  go as = prompt (map show as) msg $ \str -> case readMaybe str of
    Just a | a `elem` as -> Just a
    _ -> Nothing

-- | A confirmation message, where anything other than "y" or "yes"
-- (including empty input) is @False@.
confirm :: UI Bool
confirm = do
  setCompletions ["y", "n"]
  input <- getInputLine "Accept? [y/N] "
  pure $ case input of
    Just str -> map toLower str `elem` ["y", "yes"]
    Nothing  -> False

-- | Set the available completions
setCompletions :: [String] -> UI ()
setCompletions completions = do
  cref <- lift ask
  liftIO (writeIORef cref completions)


-------------------------------------------------------------------------------
-- (Utilities) Pretty printing

-- | Show an enemy.
showEnemy :: Colour -> State a -> ShowS
showEnemy who s = case M.lookup who (AI.enemies s) of
  Just e ->
    shows (AI.escore e) . showString " score, " . shows (AI.eremainingTrains who s) . showString " remaining trains"
  Nothing -> showString "who?"

-- | Show the additional score given by tickets.
showTicketScore :: State a -> ShowS
showTicketScore s =
  let tscore f = sum $ map AI.tvalue (f s)
  in case tscore AI.completedTickets - tscore AI.missedTickets of
    0 -> showString " (0)"
    n | n < 0 -> showAsideNum n
      | n > 0 -> showString " (+" . shows n . showString ")"

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
printList :: MonadIO m => (a -> ShowS) -> String -> [a] -> InputT m ()
printList _ none [] = outputStrLn none
printList f _ (x:xs) = do
  outputStr (f x "")
  mapM_ (\v -> outputStr . showString ", " . f v $ "") xs
  outputStrLn ""

-- | Print the plan, one route on each line, indented.
printPlan :: (MonadIO m, Show a) => State a -> InputT m ()
printPlan s = case AI.plan s of
  (p:ps) -> do
    outputStr . showString "Plan: " . showPlanItem p $ "\n"
    mapM_ (\p' -> outputStr . showString "      " . showPlanItem p' $ "\n") ps
  [] -> outputStrLn "No plan!"

-- | Print the difference between two states.
--
-- Changes to the pending and discarded tickets are not displayed, as
-- the user explicitly enters (or is told) that information.
printDiff :: (Enum a, Eq a, MonadIO m, Show a) => State a -> State a -> InputT m ()
printDiff old new = do
  when (AI.score new /= AI.score old || showTicketScore new "" /= showTicketScore old "") $
    outputStr . showString "Score: " . shows (AI.score new) . showTicketScore new $ "\n"

  when (AI.remainingTrains new /= AI.remainingTrains old) $
    outputStr . showString "Remaining trains: " . shows (AI.remainingTrains new) $ "\n"

  when (AI.remainingTurns new /= AI.remainingTurns old) $
    outputStr . showString "Min. remaining turns: " . shows (AI.remainingTurns new) $ "\n"

  for_ (M.toList $ AI.enemies new) $ \(who, e) ->
    when (Just e /= M.lookup who (AI.enemies old)) $
      outputStr . showString "Enemy " . shows who . showString ": " . showEnemy who new $ "\n"

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
      onTable' = M.insertWith (+) replacement 1 onTable
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
