Ticket to Ride AI
=================

Usage
-----

You will need a copy of Ticket to Ride: Europe, a collection of human
players, and someone to play on behalf of the AI. Play begins like so
using the Haskeline interface (starting from GHCi):

~~~haskell
> aiPlay europeState
Cards in hand: {- cards drawn into hand -}
Cards on table: {- visible cards on the table -}
Tickets: {- tickets -}

Keep these tickets: {- at least one -}

Entering game loop...
  s = suggest and play a move
  e = enemy claim   v = set visible cards
  p = print state   h = help     q = quit

ai>
~~~
