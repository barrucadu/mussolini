module Variant.Europe where

import AI (State, newState)
import Graph

data Place
  = Amsterdam
  | Angora
  | Athina
  | Barcelona
  | Berlin
  | Brest
  | Brindisi
  | Bruxelles
  | Bucuresti
  | Budapest
  | Cadiz
  | Constantinople
  | Danzig
  | Dieppe
  | Edinburgh
  | Erzurum
  | Essen
  | Frankfurt
  | Kharkov
  | Kobenhavn
  | Kyiv
  | Lisboa
  | London
  | Madrid
  | Marseille
  | Moskva
  | Munchen
  | Palermo
  | Pamplona
  | Paris
  | Petrograd
  | Riga
  | Roma
  | Rostov
  | Sarajevo
  | Sevastopol
  | Smolensk
  | Smyrna
  | Sochi
  | Sofia
  | Stockholm
  | Venezia
  | Warszawa
  | Wien
  | Wilno
  | Zagrab
  | Zurich
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Players start with 45 trains, and the 'europe' map.
europeState :: State Place
europeState = newState 45 europe

europe :: Graph Place
europe = fromList europeEdges

europeEdges :: [(Place, Place, Label)]
europeEdges =
  [ route  Amsterdam Bruxelles [Black] 1
  , route  Amsterdam Essen [Yellow] 3
  , route  Amsterdam Frankfurt [White] 2
  , ferry  Amsterdam London [Special] 2 2

  , tunnel Angora Constantinople [Special] 2
  , route  Angora Erzurum [Black] 3
  , tunnel Angora Smyrna [Orange] 3

  , ferry  Athina Brindisi [Special] 1 4
  , route  Athina Sarajevo [Green] 4
  , ferry  Athina Smyrna [Special] 1 2
  , route  Athina Sofia [Pink] 3

  , route  Barcelona Madrid [Yellow] 2
  , route  Barcelona Marseille [Special] 4
  , tunnel Barcelona Pamplona [Special] 2

  , route  Berlin Essen [Blue] 2
  , route  Berlin Danzig [Special] 4
  , route  Berlin Frankfurt [Black, Red] 3
  , route  Berlin Warszawa [Pink, Yellow] 4
  , route  Berlin Wien [Green] 3

  , route  Brest Dieppe [Orange] 2
  , route  Brest Paris [Black] 3
  , route  Brest Pamplona [Pink] 4

  , ferry  Brindisi Palermo [Special] 1 3
  , route  Brindisi Roma [White] 2

  , route  Bruxelles Dieppe [Green] 2
  , route  Bruxelles Frankfurt [Blue] 2
  , route  Bruxelles Paris [Red, Yellow] 2

  , tunnel Bucuresti Budapest [Special] 4
  , route  Bucuresti Constantinople [Yellow] 3
  , route  Bucuresti Kyiv [Special] 4
  , route  Bucuresti Sevastopol [White] 4
  , tunnel Bucuresti Sofia [Special] 2

  , tunnel Budapest Kyiv [Special] 6
  , route  Budapest Sarajevo [Pink] 3
  , route  Budapest Wien [Red, White] 1
  , route  Budapest Zagrab [Orange] 2

  , route  Cadiz Lisboa [Blue] 2
  , route  Cadiz Madrid [Orange] 3

  , ferry  Constantinople Sevastopol [Special] 2 4
  , tunnel Constantinople Smyrna [Special] 2
  , route  Constantinople Sofia [Blue] 3

  , route  Danzig Riga [Black] 3
  , route  Danzig Warszawa [Special] 2

  , ferry  Dieppe London [Special, Special] 1 2
  , route  Dieppe Paris [Pink] 1

  , route  Edinburgh London [Black, Orange] 4

  , ferry  Erzurum Sevastopol [Special] 2 4
  , tunnel Erzurum Sochi [Red] 3

  , route  Essen Frankfurt [Green] 2
  , ferry  Essen Kobenhavn [Special, Special] 1 3

  , route  Frankfurt Munchen [Pink] 2
  , route  Frankfurt Paris [Orange, White] 3

  , route  Kharkov Kyiv [Special] 4
  , route  Kharkov Moskva [Special] 4
  , route  Kharkov Rostov [Green] 2

  , route  Kobenhavn Stockholm [White, Yellow] 3

  , route  Kyiv Warszawa [Special] 4
  , route  Kyiv Wilno [Special] 2
  , route  Kyiv Smolensk [Red] 3

  , route  Lisboa Madrid [Pink] 3

  , tunnel Madrid Pamplona [Black, White] 3

  , route  Marseille Pamplona [Red] 4
  , route  Marseille Paris [Special] 4
  , tunnel Marseille Roma [Special] 4
  , tunnel Marseille Zurich [Pink] 2

  , route  Moskva Petrograd [White] 4
  , route  Moskva Smolensk [Orange] 2

  , tunnel Munchen Venezia [Blue] 2
  , route  Munchen Wien [Orange] 3
  , tunnel Munchen Zurich [Yellow] 2

  , ferry  Palermo Roma [Special] 1 4
  , ferry  Palermo Smyrna [Special] 2 6

  , route  Pamplona Paris [Blue, Green] 4

  , tunnel Paris Zurich [Special] 3

  , route  Petrograd Riga [Special] 4
  , tunnel Petrograd Stockholm [Special] 8
  , route  Petrograd Wilno [Blue] 4

  , route  Riga Wilno [Green] 4

  , route  Roma Venezia [Black] 2

  , route  Rostov Sevastopol [Special] 4
  , route  Rostov Sochi [Special] 2

  , tunnel Sarajevo Sofia [Special] 2
  , route  Sarajevo Zagrab [Red] 3

  , ferry  Sevastopol Sochi [Special] 1 2

  , route  Smolensk Wilno [Yellow] 3

  , route  Venezia Zagrab [Special] 2
  , tunnel Venezia Zurich [Green] 2

  , route  Warszawa Wien [Blue] 4
  , route  Warszawa Wilno [Red] 3

  , route  Wien Zagrab [Special] 2
  ]
