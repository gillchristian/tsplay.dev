module General.Util
  ( (&&&),
    (|||),
  )
where

import Prelude

(&&&) :: Applicative f => f Bool -> f Bool -> f Bool
f &&& g = (&&) <$> f <*> g

(|||) :: Applicative f => f Bool -> f Bool -> f Bool
f ||| g = (||) <$> f <*> g
