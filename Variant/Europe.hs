module Variant.Europe where

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

europe :: Graph Place
europe = fromList europeEdges

europeEdges :: [(Place, Place, Label)]
europeEdges =
  [ (Amsterdam, Bruxelles, Label [Just Black] 0 1)
  , (Amsterdam, Essen,     Label [Just Yellow] 0 3)
  , (Amsterdam, Frankfurt, Label [Just White] 0 2)
  , (Amsterdam, London,    Label [Nothing] 2 2)

  , (Angora, Constantinople, Label [Nothing] 0 2) -- tunnel
  , (Angora, Erzurum,        Label [Just Black] 0 3)
  , (Angora, Smyrna,         Label [Just Orange] 0 3) -- tunnel

  , (Athina, Brindisi, Label [Nothing] 1 4)
  , (Athina, Sarajevo, Label [Just Green] 0 4)
  , (Athina, Smyrna,   Label [Nothing] 1 2)
  , (Athina, Sofia,    Label [Just Pink] 0 3)

  , (Barcelona, Madrid,    Label [Just Yellow] 0 2)
  , (Barcelona, Marseille, Label [Nothing] 0 4)
  , (Barcelona, Pamplona,  Label [Nothing] 0 2) -- tunnel

  , (Berlin, Essen,     Label [Just Blue] 0 2)
  , (Berlin, Danzig,    Label [Nothing] 0 4)
  , (Berlin, Frankfurt, Label [Just Black, Just Red] 0 3)
  , (Berlin, Warszawa,  Label [Just Pink, Just Yellow] 0 4)
  , (Berlin, Wien,      Label [Just Green] 0 3)

  , (Brest, Dieppe,   Label [Just Orange] 0 2)
  , (Brest, Paris,    Label [Just Black] 0 3)
  , (Brest, Pamplona, Label [Just Pink] 0 4)

  , (Brindisi, Palermo, Label [Nothing] 1 3)
  , (Brindisi, Roma,    Label [Just White] 0 2)

  , (Bruxelles, Dieppe,    Label [Just Green] 0 2)
  , (Bruxelles, Frankfurt, Label [Just Blue] 0 2)
  , (Bruxelles, Paris,     Label [Just Red, Just Yellow] 0 2)

  , (Bucuresti, Budapest,       Label [Nothing] 0 4) -- tunnel
  , (Bucuresti, Constantinople, Label [Just Yellow] 0 3)
  , (Bucuresti, Kyiv,           Label [Nothing] 0 4)
  , (Bucuresti, Sevastopol,     Label [Just White] 0 4)
  , (Bucuresti, Sofia,          Label [Nothing] 0 2) -- tunnel

  , (Budapest, Kyiv,     Label [Nothing] 0 6) -- tunnel
  , (Budapest, Sarajevo, Label [Just Pink] 0 3)
  , (Budapest, Wien,     Label [Just Red, Just White] 0 1)
  , (Budapest, Zagrab,   Label [Just Orange] 0 2)

  , (Cadiz, Lisboa, Label [Just Blue] 0 2)
  , (Cadiz, Madrid, Label [Just Orange] 0 3)

  , (Constantinople, Sevastopol, Label [Nothing] 2 4)
  , (Constantinople, Smyrna,     Label [Nothing] 0 2) -- tunnel
  , (Constantinople, Sofia,      Label [Just Blue] 0 3)

  , (Danzig, Riga,     Label [Just Black] 0 3)
  , (Danzig, Warszawa, Label [Nothing] 0 2)

  , (Dieppe, London, Label [Nothing, Nothing] 1 2)
  , (Dieppe, Paris,  Label [Just Pink] 0 1)

  , (Edinburgh, London, Label [Just Black, Just Orange] 0 4)

  , (Erzurum, Sevastopol, Label [Nothing] 2 4)
  , (Erzurum, Sochi,      Label [Just Red] 0 3) -- tunnel

  , (Essen, Frankfurt, Label [Just Green] 0 2)
  , (Essen, Kobenhavn, Label [Nothing, Nothing] 1 3)

  , (Frankfurt, Munchen, Label [Just Pink] 0 2)
  , (Frankfurt, Paris,   Label [Just Orange, Just White] 0 3)

  , (Kharkov, Kyiv,   Label [Nothing] 0 4)
  , (Kharkov, Moskva, Label [Nothing] 0 4)
  , (Kharkov, Rostov, Label [Just Green] 0 2)

  , (Kobenhavn, Stockholm, Label [Just White, Just Yellow] 0 3)

  , (Kyiv, Warszawa, Label [Nothing] 0 4)
  , (Kyiv, Wilno,    Label [Nothing] 0 2)
  , (Kyiv, Smolensk, Label [Just Red] 0 3)

  , (Lisboa, Madrid, Label [Just Pink] 0 3)

  -- London

  , (Madrid, Pamplona, Label [Just Black, Just White] 0 3) -- tunnel

  , (Marseille, Pamplona, Label [Just Red] 0 4)
  , (Marseille, Paris,    Label [Nothing] 0 4)
  , (Marseille, Roma,     Label [Nothing] 0 4) -- tunnel
  , (Marseille, Zurich,   Label [Just Pink] 0 2) -- tunnel

  , (Moskva, Petrograd, Label [Just White] 0 4)
  , (Moskva, Smolensk,  Label [Just Orange] 0 2)

  , (Munchen, Venezia, Label [Just Blue] 0 2) -- tunnel
  , (Munchen, Wien,    Label [Just Orange] 0 3)
  , (Munchen, Zurich,  Label [Just Yellow] 0 2) -- tunnel

  , (Palermo, Roma,   Label [Nothing] 1 4)
  , (Palermo, Smyrna, Label [Nothing] 2 6)

  , (Pamplona, Paris, Label [Just Blue, Just Green] 0 4)

  , (Paris, Zurich, Label [Nothing] 0 3) -- tunnel

  , (Petrograd, Riga,      Label [Nothing] 0 4)
  , (Petrograd, Stockholm, Label [Nothing] 0 8) -- tunnel
  , (Petrograd, Wilno,     Label [Just Blue] 0 4)

  , (Riga, Wilno, Label [Just Green] 0 4)

  , (Roma, Venezia, Label [Just Black] 0 2)

  , (Rostov, Sevastopol, Label [Nothing] 0 4)
  , (Rostov, Sochi,      Label [Nothing] 0 2)

  , (Sarajevo, Sofia,  Label [Nothing] 0 2) -- tunnel
  , (Sarajevo, Zagrab, Label [Just Red] 0 3)

  , (Sevastopol, Sochi, Label [Nothing] 1 2)

  , (Smolensk, Wilno, Label [Just Yellow] 0 3)

  , (Venezia, Zagrab, Label [Nothing] 0 2)
  , (Venezia, Zurich, Label [Just Green] 0 2) -- tunnel

  , (Warszawa, Wien,  Label [Just Blue] 0 4)
  , (Warszawa, Wilno, Label [Just Red] 0 3)

  , (Wien, Zagrab, Label [Nothing] 0 2)
  ]
