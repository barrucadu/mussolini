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
  [ (Amsterdam, Bruxelles, Label [Black] 0 1)
  , (Amsterdam, Essen,     Label [Yellow] 0 3)
  , (Amsterdam, Frankfurt, Label [White] 0 2)
  , (Amsterdam, London,    Label [Special] 2 2)

  , (Angora, Constantinople, Label [Special] 0 2) -- tunnel
  , (Angora, Erzurum,        Label [Black] 0 3)
  , (Angora, Smyrna,         Label [Orange] 0 3) -- tunnel

  , (Athina, Brindisi, Label [Special] 1 4)
  , (Athina, Sarajevo, Label [Green] 0 4)
  , (Athina, Smyrna,   Label [Special] 1 2)
  , (Athina, Sofia,    Label [Pink] 0 3)

  , (Barcelona, Madrid,    Label [Yellow] 0 2)
  , (Barcelona, Marseille, Label [Special] 0 4)
  , (Barcelona, Pamplona,  Label [Special] 0 2) -- tunnel

  , (Berlin, Essen,     Label [Blue] 0 2)
  , (Berlin, Danzig,    Label [Special] 0 4)
  , (Berlin, Frankfurt, Label [Black, Red] 0 3)
  , (Berlin, Warszawa,  Label [Pink, Yellow] 0 4)
  , (Berlin, Wien,      Label [Green] 0 3)

  , (Brest, Dieppe,   Label [Orange] 0 2)
  , (Brest, Paris,    Label [Black] 0 3)
  , (Brest, Pamplona, Label [Pink] 0 4)

  , (Brindisi, Palermo, Label [Special] 1 3)
  , (Brindisi, Roma,    Label [White] 0 2)

  , (Bruxelles, Dieppe,    Label [Green] 0 2)
  , (Bruxelles, Frankfurt, Label [Blue] 0 2)
  , (Bruxelles, Paris,     Label [Red, Yellow] 0 2)

  , (Bucuresti, Budapest,       Label [Special] 0 4) -- tunnel
  , (Bucuresti, Constantinople, Label [Yellow] 0 3)
  , (Bucuresti, Kyiv,           Label [Special] 0 4)
  , (Bucuresti, Sevastopol,     Label [White] 0 4)
  , (Bucuresti, Sofia,          Label [Special] 0 2) -- tunnel

  , (Budapest, Kyiv,     Label [Special] 0 6) -- tunnel
  , (Budapest, Sarajevo, Label [Pink] 0 3)
  , (Budapest, Wien,     Label [Red, White] 0 1)
  , (Budapest, Zagrab,   Label [Orange] 0 2)

  , (Cadiz, Lisboa, Label [Blue] 0 2)
  , (Cadiz, Madrid, Label [Orange] 0 3)

  , (Constantinople, Sevastopol, Label [Special] 2 4)
  , (Constantinople, Smyrna,     Label [Special] 0 2) -- tunnel
  , (Constantinople, Sofia,      Label [Blue] 0 3)

  , (Danzig, Riga,     Label [Black] 0 3)
  , (Danzig, Warszawa, Label [Special] 0 2)

  , (Dieppe, London, Label [Special, Special] 1 2)
  , (Dieppe, Paris,  Label [Pink] 0 1)

  , (Edinburgh, London, Label [Black, Orange] 0 4)

  , (Erzurum, Sevastopol, Label [Special] 2 4)
  , (Erzurum, Sochi,      Label [Red] 0 3) -- tunnel

  , (Essen, Frankfurt, Label [Green] 0 2)
  , (Essen, Kobenhavn, Label [Special, Special] 1 3)

  , (Frankfurt, Munchen, Label [Pink] 0 2)
  , (Frankfurt, Paris,   Label [Orange, White] 0 3)

  , (Kharkov, Kyiv,   Label [Special] 0 4)
  , (Kharkov, Moskva, Label [Special] 0 4)
  , (Kharkov, Rostov, Label [Green] 0 2)

  , (Kobenhavn, Stockholm, Label [White, Yellow] 0 3)

  , (Kyiv, Warszawa, Label [Special] 0 4)
  , (Kyiv, Wilno,    Label [Special] 0 2)
  , (Kyiv, Smolensk, Label [Red] 0 3)

  , (Lisboa, Madrid, Label [Pink] 0 3)

  -- London

  , (Madrid, Pamplona, Label [Black, White] 0 3) -- tunnel

  , (Marseille, Pamplona, Label [Red] 0 4)
  , (Marseille, Paris,    Label [Special] 0 4)
  , (Marseille, Roma,     Label [Special] 0 4) -- tunnel
  , (Marseille, Zurich,   Label [Pink] 0 2) -- tunnel

  , (Moskva, Petrograd, Label [White] 0 4)
  , (Moskva, Smolensk,  Label [Orange] 0 2)

  , (Munchen, Venezia, Label [Blue] 0 2) -- tunnel
  , (Munchen, Wien,    Label [Orange] 0 3)
  , (Munchen, Zurich,  Label [Yellow] 0 2) -- tunnel

  , (Palermo, Roma,   Label [Special] 1 4)
  , (Palermo, Smyrna, Label [Special] 2 6)

  , (Pamplona, Paris, Label [Blue, Green] 0 4)

  , (Paris, Zurich, Label [Special] 0 3) -- tunnel

  , (Petrograd, Riga,      Label [Special] 0 4)
  , (Petrograd, Stockholm, Label [Special] 0 8) -- tunnel
  , (Petrograd, Wilno,     Label [Blue] 0 4)

  , (Riga, Wilno, Label [Green] 0 4)

  , (Roma, Venezia, Label [Black] 0 2)

  , (Rostov, Sevastopol, Label [Special] 0 4)
  , (Rostov, Sochi,      Label [Special] 0 2)

  , (Sarajevo, Sofia,  Label [Special] 0 2) -- tunnel
  , (Sarajevo, Zagrab, Label [Red] 0 3)

  , (Sevastopol, Sochi, Label [Special] 1 2)

  , (Smolensk, Wilno, Label [Yellow] 0 3)

  , (Venezia, Zagrab, Label [Special] 0 2)
  , (Venezia, Zurich, Label [Green] 0 2) -- tunnel

  , (Warszawa, Wien,  Label [Blue] 0 4)
  , (Warszawa, Wilno, Label [Red] 0 3)

  , (Wien, Zagrab, Label [Special] 0 2)
  ]
