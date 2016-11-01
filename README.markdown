Ticket to Ride AI
=================

Usage
-----

You will need a copy of Ticket to Ride: Europe, a collection of human
players, and someone to play on behalf of the AI. A sample play
session begins like so:

~~~
$ stack build
$ stack exec ttrai
Cards in hand: Red White Blue Blue
Cards on table: Green Green Special Orange White
Tickets: Cadiz Stockholm 21 Berlin Bucuresti 8 Brest Marseille 7 Palermo Constantinople 8

Keep these tickets: Cadiz -> Stockholm (21), Palermo -> Constantinople (8)

Entering game loop...
  s = suggest and play a move
  c = claim   d = discard   e = enemy claim
  T = set table cards   H = set hand cards   R = set remaining trains
  p = print state   h = help   q = quit

ai> p
Remaining trains: 45

Cards:
        In hand:  White (1), Blue (2), Red (1)
        On table: White (1), Orange (1), Green (2), Special (1)

Tickets:
        Complete: none!
        Pending:  Cadiz -> Stockholm (21), Palermo -> Constantinople (8)
        Missed:   none!

Plan: Cadiz -> Madrid (3)
      Madrid -> Pamplona (3)
      Pamplona -> Paris (4)
      Paris -> Frankfurt (3)
      Frankfurt -> Essen (2)
      Essen -> Kobenhavn (3)
      Kobenhavn -> Stockholm (3)
      Palermo -> Smyrna (6)
      Smyrna -> Constantinople (2)

ai>
~~~
