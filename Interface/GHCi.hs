{-# LANGUAGE LambdaCase #-}

module Interface.GHCi (NonEmpty(..), newAI) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as L
import qualified Data.Map as M
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Text.Show (shows, showString)

import AI (Move, State, Ticket(..))
import Graph (Colour(Special), Label(lweight))
import qualified AI

-- | GHCi interface to the AI.
--
-- @
-- (aiPlay, aiSuggest, aiPlan, aiReplan, aiDo, aiPrintState) <- newAI state
-- @
--
-- Then call the bound functions to interact with the game,
-- automatically updating the game state.
newAI :: (Enum a, Eq a, Read a, Show a) => State a
  -> IO ( IO ()
        , IO (Move a)
        , NonEmpty (Ticket a) -> IO (NonEmpty (Ticket a))
        , IO ()
        , (State a -> State a) -> IO ()
        , IO ()
        )
newAI s = do
  ref <- newIORef s

  pure ( aiPlay       ref
       , aiSuggest    ref
       , aiPlan       ref
       , aiReplan     ref
       , modifyIORef  ref
       , aiPrintState ref
       )

-- | Play the game!
aiPlay :: (Enum a, Eq a, Read a, Show a) => IORef (State a) -> IO ()
aiPlay ref = do
    initialise

    putStrLn "\nEntering game loop..."
    help
    loop Nothing
  where
    -- set up the initial game state.
    initialise = do
      theHand  <- prompt "Cards in hand: "  readWords
      theTable <- prompt "Cards on table: " readWords

      modifyIORef ref $ AI.draw     theHand
      modifyIORef ref $ AI.setCards theTable
      getTickets

    -- a brief help message
    help = do
      putStrLn "  s = suggest       a = take last suggestion"
      putStrLn "  e = enemy claim   v = set visible cards"
      putStrLn "  p = print state   h = help     q = quit"

    -- game loop: wait for input, suggest a move, wait for a response, loop
    loop lastaction = putStrLn "" >> prompt "ai> " Just >>= \case
      "s" -> do
        action <- aiSuggest ref
        print action
        loop (Just action)
      "a" -> do
        case lastaction of
          Just AI.DrawLocomotiveCard -> doDrawSpecial
          Just (AI.DrawCards c1 c2) -> doDraw c1 c2
          Just (AI.ClaimRoute from to colour cards) -> doClaim from to colour cards
          Just AI.DrawTickets -> doDrawTickets
          Nothing -> putStrLn "No last action!"
        loop Nothing
      "e" -> doEnemyClaim
      "v" -> doSetCards
      "p" -> aiPrintState ref >> loop lastaction
      "h" -> help >> loop lastaction
      "q" -> pure ()
      _   -> loop lastaction

    -- draw a locomotive
    doDrawSpecial = do
      replacement <- prompt "Replacement card: " readMaybe
      replaceDraw Special replacement
      printCards

    -- draw a pair of cards
    doDraw (Just c1) (Just c2) = do
      replacements <- prompt "Replacement cards: " (readWordsL 2)
      replaceDraw c1 (head replacements)
      replaceDraw c2 (replacements !! 1)
      printCards
    doDraw (Just c) _ = do
      fromdeck    <- prompt "Card from deck:   " readMaybe
      replacement <- prompt "Replacement card: " readMaybe
      replaceDraw c replacement
      modifyIORef ref $ AI.draw [fromdeck]
      printCards
    doDraw _ _ = do
      fromdeck <- prompt "Cards from deck: " (readWordsL 2)
      modifyIORef ref $ AI.draw fromdeck
      printCards

    -- claim a route
    doClaim from to colour cards = do
      s <- readIORef ref
      let s' = AI.discard cards $ AI.claim from to colour s
      when (AI.plan s /= AI.plan s') (printPlan s')
      writeIORef ref s'

    -- draw tickets
    doDrawTickets = do
      getTickets
      putStrLn ""
      printPlan =<< readIORef ref

    -- register an enemy claim
    doEnemyClaim = do
      from   <- prompt "From:   " readMaybe
      to     <- prompt "To:     " readMaybe
      colour <- prompt "Colour: " readMaybe

      s <- readIORef ref
      let s' = AI.enemyClaim from to colour s
      when (AI.plan s /= AI.plan s') (printPlan s')
      writeIORef ref s'

    -- set the visible cards
    doSetCards = do
      theTable <- prompt "Cards on table: " readWords
      modifyIORef ref $ AI.setCards theTable

    -- draw a card with replacement
    replaceDraw original replacement = modifyIORef ref $ \s ->
      let s' = AI.draw [original] s
          onTable  = M.update (\i -> if i > 1 then Just (i-1) else Nothing) original (AI.ontable s)
          onTable' = M.alter (\case Just i -> Just (i+1); Nothing -> Just 1) replacement onTable
      in s' { AI.ontable = onTable' }

    -- print the cards in the hand and on the table
    printCards = do
      s <- readIORef ref
      putStr "In hand:  " >> printList showCard "none!" (M.toList $ AI.hand    s)
      putStr "On table: " >> printList showCard "none!" (M.toList $ AI.ontable s)

    -- get some tickets
    getTickets = do
      theTickets <- prompt "Tickets: " readTickets
      keep <- aiPlan ref theTickets
      putStr "\nKeep these tickets: " >> printList showTicket "none!" (L.toList keep)

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

  printPlan s

-------------------------------------------------------------------------------
-- Utilities

showCard :: (Colour, Int) -> ShowS
showCard (colour, num) = showColour colour . showAsideNum num

showTicket :: Show a => Ticket a -> ShowS
showTicket (Ticket from to value) =
  shows from . showString " -> " . shows to . showAsideNum value

showPlanItem :: Show a => (a, a, Label) -> ShowS
showPlanItem (from, to, label) =
  shows from . showString " -> " . shows to . showAsideNum (lweight label)

showColour :: Colour -> ShowS
showColour Special = showString "Locomotive"
showColour c = shows c

showAsideNum :: Int -> ShowS
showAsideNum num = showString " (" . shows num . showString ")"

printList :: (a -> ShowS) -> String -> [a] -> IO ()
printList _ none [] = putStrLn none
printList f _ (x:xs) = do
  putStr (f x "")
  mapM_ (\v -> putStr . showString ", " . f v $ "") xs
  putStr "\n"

printPlan :: Show a => State a -> IO ()
printPlan s = case AI.plan s of
  (p:ps) -> do
    putStrLn . showString "Plan: " . showPlanItem p $ ""
    mapM_ (\p' -> putStrLn . showString "      " . showPlanItem p' $ "") ps
  [] -> putStrLn "No plan!"

prompt :: String -> (String -> Maybe a) -> IO a
prompt msg f = go where
  go = do
    putStr msg
    hFlush stdout
    input <- f <$> getLine
    maybe (putStrLn "Try again." >> go) pure input

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
