Ticket to Ride AI
=================

Usage
-----

You will need a copy of Ticket to Ride: Europe, a collection of human
players, and someone to play on behalf of the AI. Play begins like so
using the GHCi interface:

~~~haskell
> (aiSuggest, aiPlan, aiReplan, aiDo, aiPrintState) <- newAI europeState

> aiDo $ setCards [ {- visible cards on the table -} ]

> aiDo $ draw [ {- cards drawn into hand -} ]

> aiPlan $ {- long ticket -} :| [ {- remaining tickets -} ]
  -- only keep the tickets it picks
~~~

The commands available are:

- `aiSuggest`: suggest the next move to perform.
- `aiPlan $ ... :| [ ... ]`: produce a plan from the given tickets and
  the currently-held pending ones; returns the tickets to keep.
- `aiReplan`: recompute the plan for the currently-held pending
  tickets.
- `aiDo $ setCards [ ... ]`: define the cards on the table.
- `aiDo $ draw [ ... ]`: draw cards into the hand.
- `aiDo $ discard [ ... ]`: discard cards from the hand.
- `aiDo $ claim ...`: claim a route.
- `aiDo $ claimSingle ...`: claim a route without specifying the
  colour (only works if there are no parallel routes).
- `aiDo $ enemyClaim ...`: indicate that an enemy has claimed a route.
- `aiDo $ enemyClaimSingle ...`: indicate that an enemy has claimed a
  route without specifying the colour (only works if there are no
  parallel routes).
- `aiPrintState`: print a representation of the AI's state and plan.

Before asking for a move suggestion, make sure the cards in the hand
and on the table are correct.
