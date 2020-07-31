module General.Util
  ( (<&>),
    (<|>),
  )
where

import Control.Applicative (liftA2)
import Prelude

(<&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&>) = liftA2 (&&)

(<|>) :: Applicative f => f Bool -> f Bool -> f Bool
(<|>) = liftA2 (||)
